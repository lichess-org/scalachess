package chess
package variant

import cats.data.Validated
import cats.syntax.option._
import chess.format.{FEN, Uci}

case object NewChess1
    extends Variant(
      id = 11,
      key = "newchess1",
      name = "NewChess1",
      shortName = "NewChess1",
      title = "A game with Doom piece started from the off board. It can be placed instead of moving a piece",
      standardInitialPosition = true
    ) {

  override def isValidPromotion(promotion: Option[PromotableRole]) =
    promotion match {
      case None                                 => true
      case Some(Queen | Rook | Knight | Bishop| Doom) => true
      case _                                    => false
    }

  override val roles = List(Rook, Knight, King, Bishop, King, Queen, Pawn, Doom)

  override val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, Doom)

  def pieces = Standard.pieces

  override val initialFen = FEN(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"
  )

  override def valid(board: Board, strict: Boolean) = {
    (Color.all forall validSide(board, false)) && !strict
  }

  // todo: is position empty?
  private def canDropDoomOn(pos: Pos, color: Color) = {
    pos.rank == color.backRank && (pos.file == File.E || pos.file == File.D)
  }

  override def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    for {
      d1 <- situation.board.newChess1Data toValid "Board has no newchess1 data"
      _ <-
        if (role != Doom || canDropDoomOn(pos, situation.color)) Validated.valid(d1)
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
      after = board1 withNewChess1Data d2
    )

  // there is always sufficient mating material in Crazyhouse
  override def opponentHasInsufficientMaterial(situation: Situation) = InsufficientMatingMaterial(situation.board, !situation.color) && (if (!situation.color == Color.white) situation.board.newChess1Data.forall(_.pockets.white.roles.isEmpty) else situation.board.newChess1Data.forall(_.pockets.black.roles.isEmpty))
  override def isInsufficientMaterial(board: Board)                  = InsufficientMatingMaterial(board) && (board.newChess1Data.forall(_.pockets.white.roles.isEmpty) && board.newChess1Data.forall(_.pockets.black.roles.isEmpty))

  override def fiftyMoves(history: History): Boolean = false

  private def canDropStuff(situation: Situation) =
    situation.board.newChess1Data.fold(false) { (data: Data) =>
      val roles = data.pockets(situation.color).roles
      roles.nonEmpty && possibleDrops(situation).fold(true) { squares =>
        squares.nonEmpty && squares.exists(s => canDropDoomOn(s, situation.color))
      }
    }

  override def staleMate(situation: Situation) =
    super.staleMate(situation) && !canDropStuff(situation)

  override def checkmate(situation: Situation) =
    super.checkmate(situation) && !canDropStuff(situation)

  def possibleDrops(situation: Situation): Option[List[Pos]] =
    if (!situation.check) None
    else situation.kingPos.map { blockades(situation, _) }

  private def blockades(situation: Situation, kingPos: Pos): List[Pos] = {
    def attacker(piece: Piece) = piece.role.projection && piece.color != situation.color
    @scala.annotation.tailrec
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

  case class Data(
      pockets: Pockets
  ) {

    def drop(piece: Piece): Option[Data] =
      pockets take piece map { nps =>
        copy(pockets = nps)
      }
  }

  object Data {
    val init = Data(Pockets(Pocket(List(Doom)), Pocket(List(Doom))))
  }

  case class Pockets(white: Pocket, black: Pocket) {

    def apply(color: Color) = color.fold(white, black)

    def take(piece: Piece): Option[Pockets] =
      piece.role match {
        case Doom =>
          piece.color.fold(
            white take piece.role map { np =>
              copy(white = np)
            },
            black take piece.role map { np =>
              copy(black = np)
            }
          )
        case _ => None
      }
  }

  case class Pocket(roles: List[Role]) {

    def take(role: Role) =
      if (roles contains role) Option(copy(roles = roles diff List(role)))
      else None
  }
}
