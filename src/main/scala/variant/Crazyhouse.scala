package chess
package variant

import chess.format.Uci
import cats.syntax.option._
import cats.data.Validated

case object Crazyhouse
    extends Variant(
      id = 10,
      key = "crazyhouse",
      name = "Crazyhouse",
      shortName = "Crazy",
      title = "Captured pieces can be dropped back on the board instead of moving a piece.",
      standardInitialPosition = true
    ) {

  def pieces = Standard.pieces

  override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"

  override def valid(board: Board, strict: Boolean) = {
    val pieces = board.pieces.values
    (Color.all forall validSide(board, false) _) &&
    (!strict || (pieces.count(_ is Pawn) <= 16 && pieces.size <= 32))
  }

  private def canDropPawnOn(pos: Pos) = pos.y != 1 && pos.y != 8

  override def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    for {
      d1 <- situation.board.crazyData toValid "Board has no crazyhouse data"
      _ <-
        if (role != Pawn || canDropPawnOn(pos)) Validated.valid(d1)
        else Validated.invalid(s"Can't drop $role on $pos")
      piece = Piece(situation.color, role)
      d2     <- d1.drop(piece) toValid s"No $piece to drop on $pos"
      board1 <- situation.board.place(piece, pos) toValid s"Can't drop $role on $pos, it's occupied"
      _ <-
        if (!board1.check(situation.color)) Validated.valid(board1)
        else Validated.invalid(s"Dropping $role on $pos doesn't uncheck the king")
    } yield Drop(
      piece = piece,
      pos = pos,
      situationBefore = situation,
      after = board1 withCrazyData d2
    )

  override def fiftyMoves(history: History): Boolean = false

  override def isIrreversible(move: Move): Boolean = move.castles

  override def finalizeBoard(board: Board, uci: Uci, capture: Option[Piece]): Board =
    uci match {
      case Uci.Move(orig, dest, promOption) =>
        board.crazyData.fold(board) { data =>
          val d1 = capture.fold(data) { data.store(_, dest) }
          val d2 = promOption.fold(d1.move(orig, dest)) { _ =>
            d1 promote dest
          }
          board withCrazyData d2
        }
      case _ => board
    }

  private def canDropStuff(situation: Situation) =
    situation.board.crazyData.fold(false) { (data: Data) =>
      val roles = data.pockets(situation.color).roles
      roles.nonEmpty && possibleDrops(situation).fold(true) { squares =>
        squares.nonEmpty && {
          squares.exists(canDropPawnOn) || roles.exists(chess.Pawn !=)
        }
      }
    }

  override def staleMate(situation: Situation) =
    super.staleMate(situation) && !canDropStuff(situation)

  override def checkmate(situation: Situation) =
    super.checkmate(situation) && !canDropStuff(situation)

  // there is always sufficient mating material in Crazyhouse
  override def opponentHasInsufficientMaterial(situation: Situation) = false
  override def isInsufficientMaterial(board: Board)                  = false

  def possibleDrops(situation: Situation): Option[List[Pos]] =
    if (!situation.check) None
    else situation.kingPos.map { blockades(situation, _) }

  private def blockades(situation: Situation, kingPos: Pos): List[Pos] = {
    def attacker(piece: Piece) = piece.role.projection && piece.color != situation.color
    def forward(p: Pos, dir: Direction, squares: List[Pos]): List[Pos] =
      dir(p) match {
        case None                                                 => Nil
        case Some(next) if situation.board(next).exists(attacker) => next :: squares
        case Some(next) if situation.board(next).isDefined        => Nil
        case Some(next)                                           => forward(next, dir, next :: squares)
      }
    Queen.dirs flatMap { forward(kingPos, _, Nil) } filter { square =>
      situation.board.place(Piece(situation.color, Knight), square) exists { defended =>
        !defended.check(situation.color)
      }
    }
  }

  val storableRoles = List(Pawn, Knight, Bishop, Rook, Queen)

  case class Data(
      pockets: Pockets,
      // in crazyhouse, a promoted piece becomes a pawn
      // when captured and put in the pocket.
      // there we need to remember which pieces are issued from promotions.
      // we do that by tracking their positions on the board.
      promoted: Set[Pos]
  ) {

    def drop(piece: Piece): Option[Data] =
      pockets take piece map { nps =>
        copy(pockets = nps)
      }

    def store(piece: Piece, from: Pos) =
      copy(
        pockets = pockets store {
          if (promoted(from)) piece.color.pawn else piece
        },
        promoted = promoted - from
      )

    def promote(pos: Pos) = copy(promoted = promoted + pos)

    def move(orig: Pos, dest: Pos) =
      copy(
        promoted = if (promoted(orig)) promoted - orig + dest else promoted
      )
  }

  object Data {
    val init = Data(Pockets(Pocket(Nil), Pocket(Nil)), Set.empty)
  }

  case class Pockets(white: Pocket, black: Pocket) {

    def apply(color: Color) = color.fold(white, black)

    def take(piece: Piece): Option[Pockets] =
      piece.color.fold(
        white take piece.role map { np =>
          copy(white = np)
        },
        black take piece.role map { np =>
          copy(black = np)
        }
      )

    def store(piece: Piece) =
      piece.color.fold(
        copy(black = black store piece.role),
        copy(white = white store piece.role)
      )
  }

  case class Pocket(roles: List[Role]) {

    def take(role: Role) =
      if (roles contains role) Option(copy(roles = roles diff List(role)))
      else None

    def store(role: Role) =
      if (storableRoles contains role) copy(roles = role :: roles)
      else this
  }
}
