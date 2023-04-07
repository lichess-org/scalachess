package chess
package variant

import chess.format.Uci
import cats.syntax.option.*
import cats.data.Validated
import chess.format.EpdFen
import bitboard.Bitboard

case object Crazyhouse
    extends Variant(
      id = Variant.Id(10),
      key = Variant.LilaKey("crazyhouse"),
      uciKey = Variant.UciKey("crazyhouse"),
      name = "Crazyhouse",
      shortName = "Crazy",
      title = "Captured pieces can be dropped back on the board instead of moving a piece.",
      standardInitialPosition = true
    ):

  def pieces = Standard.pieces

  override val initialFen = EpdFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1")

  def validMoves(situation: Situation): List[Move] =
    Standard.validMoves(situation)

  override def valid(board: Board, strict: Boolean) =
    (Color.all forall validSide(board, false)) &&
      (!strict || (board.board.byRole(Pawn).count <= 16 && board.nbPieces <= 32))

  private def canDropPawnOn(pos: Pos) = pos.rank != Rank.First && pos.rank != Rank.Eighth

  override def drop(situation: Situation, role: Role, pos: Pos): Validated[ErrorStr, Drop] =
    for
      d1 <- situation.board.crazyData toValid ErrorStr("Board has no crazyhouse data")
      _ <-
        if (role != Pawn || canDropPawnOn(pos)) Validated.valid(d1)
        else Validated.invalid(ErrorStr(s"Can't drop $role on $pos"))
      piece = Piece(situation.color, role)
      d2     <- d1.drop(piece) toValid ErrorStr(s"No $piece to drop on $pos")
      board1 <- situation.board.place(piece, pos) toValid ErrorStr(s"Can't drop $role on $pos, it's occupied")
      _ <-
        if board1.checkOf(situation.color).no then Validated.valid(board1)
        else Validated.invalid(ErrorStr(s"Dropping $role on $pos doesn't uncheck the king"))
    yield Drop(
      piece = piece,
      pos = pos,
      situationBefore = situation,
      after = board1 withCrazyData d2
    )

  override def fiftyMoves(history: History): Boolean = false

  override def isIrreversible(move: Move): Boolean = move.castles

  override def finalizeBoard(board: Board, uci: Uci, capture: Option[Piece]): Board =
    uci match
      case Uci.Move(orig, dest, promOption) =>
        board.crazyData.fold(board) { data =>
          val d1 = capture.fold(data) { data.store(_, dest) }
          val d2 = promOption.fold(d1.move(orig, dest)) { _ =>
            d1 promote dest
          }
          board withCrazyData d2
        }
      case _ => board

  private def canDropStuff(situation: Situation) =
    situation.board.crazyData.exists { (data: Data) =>
      val pocket = data.pockets(situation.color)
      roles.nonEmpty && possibleDrops(situation).fold(true) { squares =>
        squares.nonEmpty && {
          squares.exists(canDropPawnOn) || pocket.hasNonPawn
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

  // if the king is not in check, all drops are possible, we just return None
  // king is in single check, we return the squares between the king and the checker
  // king is in double check, no drop is possible
  def possibleDrops(situation: Situation): Option[List[Pos]] =
    situation.ourKing.flatMap(king =>
      val checkers = situation.board.board.attackers(king, !situation.color)
      if checkers.moreThanOne then Some(Nil)
      else checkers.first.map(Bitboard.between(king, _).squares)
    )

  // all legal moves and drops
  // this function is used in perfts only
  def legalMoves(situation: Situation): List[MoveOrDrop] =
    legalDrops(situation) ::: situation.legalMoves.filterNot(m =>
      m.castle.exists(c => c.isStandard && m.dest != c.rook)
    )

  // if the king is not in check, all empty squares are possible drop
  // king is in single check, return the squares between the king and the checker
  // king is in double check, no drop is possible
  // this function is used in perfts only
  private def legalDropSquares(situation: Situation): Bitboard =
    situation.ourKing
      .map(king =>
        val checkers = situation.board.board.attackers(king, !situation.color)
        if checkers.isEmpty then ~situation.board.occupied
        else checkers.singleSquare.map(Bitboard.between(king, _)).getOrElse(Bitboard.empty)
      )
      .getOrElse(Bitboard.empty)

  // generate all legal drops
  // this function is used in perfts only
  private def legalDrops(situation: Situation): List[Drop] =
    val targets = legalDropSquares(situation)
    if targets.isEmpty then Nil
    else
      situation.board.crazyData.fold(List.empty[Drop]) { data =>
        val pocket = data.pockets(situation.color)
        val dropsWithoutPawn =
          for
            role <- List(Knight, Bishop, Rook, Queen)
            if pocket contains role
            to <- targets.squares
            piece = Piece(situation.color, role)
            after = situation.board.place(piece, to).get // this is safe, we checked the target squares
            d2    = data.drop(piece).get                 // this is safe, we checked the pocket
          yield Drop(piece, to, situation, after withCrazyData d2)
        val dropWithPawn =
          if pocket contains Pawn then
            for
              to <- (targets & ~Bitboard.firstRank & ~Bitboard.lastRank).squares
              piece = Piece(situation.color, Pawn)
              after = situation.board.place(piece, to).get // this is safe, we checked the target squares
              d2    = data.drop(piece).get                 // this is safe, we checked the pocket
            yield Drop(piece, to, situation, after withCrazyData d2)
          else Nil
        dropsWithoutPawn ::: dropWithPawn
      }

  val storableRoles: Set[Role] = Set(Pawn, Knight, Bishop, Rook, Queen)

  type Pockets = ByColor[Pocket]
  case class Data(
      pockets: Pockets,
      // in crazyhouse, a promoted piece becomes a pawn
      // when captured and put in the pocket.
      // there we need to remember which pieces are issued from promotions.
      // we do that by tracking their positions on the board.
      promoted: Bitboard
  ):

    import bitboard.Bitboard.bb
    def drop(piece: Piece): Option[Data] =
      pockets take piece map { nps =>
        copy(pockets = nps)
      }

    def store(piece: Piece, from: Pos): Data =
      copy(
        pockets = pockets store {
          if promoted.contains(from) then piece.color.pawn else piece
        },
        promoted = promoted.removeSquare(from)
      )

    def promote(pos: Pos) = copy(promoted = promoted.addSquare(pos))

    def move(orig: Pos, dest: Pos) =
      copy(
        promoted = if promoted.contains(orig) then promoted.removeSquare(orig).addSquare(dest) else promoted
      )

    def isEmpty = pockets.white.isEmpty && pockets.black.isEmpty
    def size    = pockets.white.size + pockets.black.size

  object Data:
    val init = Data(ByColor(Pocket.empty), Bitboard.empty)

  extension (pockets: Pockets)

    def take(piece: Piece): Option[Pockets] =
      pockets(piece.color).take(piece.role).map { np =>
        pockets.update(piece.color, _ => np)
      }

    def store(piece: Piece): Pockets =
      pockets.update(!piece.color, _.store(piece.role))

  case class Pocket(pawn: Natural, knight: Natural, bishop: Natural, rook: Natural, queen: Natural):

    def forsythUpper = forsyth.toUpperCase
    def forsyth: String = forsyth(pawn, 'p') + forsyth(knight, 'n') +
      forsyth(bishop, 'b') + forsyth(rook, 'r') + forsyth(queen, 'q')

    def forsyth(role: Int, char: Char) = List.fill(role)(char).mkString

    def roles(count: Int, role: Role): List[Role] = List.fill(count)(role)
    def roles: List[Role] = roles(pawn, Pawn) ::: roles(knight, Knight) ::: roles(bishop, Bishop) :::
      roles(rook, Rook) ::: roles(queen, Queen)

    def size       = pawn + knight + bishop + rook + queen
    def isEmpty    = size == 0
    def nonEmpty   = size > 0
    def hasPawn    = pawn > 0
    def hasNonPawn = knight + bishop + rook + queen > 0

    def contains(r: Role): Boolean = r match
      case Pawn   => pawn > 0
      case Knight => knight > 0
      case Bishop => bishop > 0
      case Rook   => rook > 0
      case Queen  => queen > 0
      case King   => false

    def apply(role: Role): Option[Natural] =
      role match
        case Pawn   => Some(pawn)
        case Knight => Some(knight)
        case Bishop => Some(bishop)
        case Rook   => Some(rook)
        case Queen  => Some(queen)
        case King   => None

    def take(role: Role): Option[Pocket] =
      update(role, (x => if x > 0 then Some(x - 1) else None))

    def store(role: Role): Pocket = update(role, _ + 1)

    def update(role: Role, f: Natural => Natural): Pocket = role match
      case Pawn   => copy(pawn = f(pawn))
      case Knight => copy(knight = f(knight))
      case Bishop => copy(bishop = f(bishop))
      case Rook   => copy(rook = f(rook))
      case Queen  => copy(queen = f(queen))
      case King   => this

    def update(role: Role, f: Natural => Option[Natural]): Option[Pocket] = role match
      case Pawn   => f(pawn).map(x => copy(pawn = x))
      case Knight => f(knight).map(x => copy(knight = x))
      case Bishop => f(bishop).map(x => copy(bishop = x))
      case Rook   => f(rook).map(x => copy(rook = x))
      case Queen  => f(queen).map(x => copy(queen = x))
      case King   => None

  type Natural = Int

  object Pocket:
    val empty = Pocket(0, 0, 0, 0, 0)
    def apply(roles: List[Role]): Pocket =
      roles.foldLeft(empty) { (p, r) =>
        p store r
      }

  // opaque type Natural <: Int = Int
  // def Natural(x: Int): Option[Natural] =
  //   if x >= 0 then Some(x) else None
  //
  // extension (n: Natural) def -(m: Int): Option[Natural] = Natural(n - m)
