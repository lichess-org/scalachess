package chess

import cats.syntax.all.*

import scala.language.implicitConversions

import format.{ FullFen, Fen, Uci, Visual }
import format.pgn.PgnStr
import variant.{ Chess960, Variant }
import bitboard.Board as BBoard

trait ChessTestCommon:

  given Conversion[String, Board]  = Visual.<<
  given Conversion[String, PgnStr] = PgnStr(_)
  given Conversion[PgnStr, String] = _.value

  extension (str: String)
    def chess960: Board             = makeBoard(str, chess.variant.Chess960)
    def kingOfTheHill: Board        = makeBoard(str, chess.variant.KingOfTheHill)
    def threeCheck: Board           = makeBoard(str, chess.variant.ThreeCheck)
    def as(color: Color): Situation = Situation(Visual << str, color)

  extension (board: Board)
    def visual = Visual >> board
    def destsFrom(from: Square): Option[List[Square]] =
      board(from).map: piece =>
        Situation(board, piece.color).generateMovesAt(from).map(_.dest)

    def seq(actions: Board => Option[Board]*): Option[Board] =
      actions.foldLeft(board.some)(_ flatMap _)

  extension (game: Game)
    def as(color: Color): Game = game.withPlayer(color)

    def playMoves(moves: (Square, Square)*): Either[ErrorStr, Game] = playMoveList(moves)

    def playMoveList(moves: Iterable[(Square, Square)]): Either[ErrorStr, Game] =
      moves.toList.foldM(game):
        case (game, (o, d)) => game.playMove(o, d)

    def playMove(
        orig: Square,
        dest: Square,
        promotion: Option[PromotableRole] = None
    ): Either[ErrorStr, Game] =
      game(orig, dest, promotion).map(_._1)

    def withClock(c: Clock) = game.copy(clock = Option(c))

  extension (sit: Situation)
    def movesAt(s: Square): List[Move] =
      sit.moves.getOrElse(s, Nil)

  def fenToGameEither(positionString: FullFen, variant: Variant): Either[String, Game] =
    Fen
      .read(variant, positionString)
      .map: sit =>
        sit.color -> sit.withVariant(variant).board
      .map: (color, board) =>
        Game(variant).copy(situation = Situation(board, color))
      .toRight("Could not construct situation from Fen")

  def makeBoard(pieces: (Square, Piece)*): Board =
    Board(BBoard.fromMap(pieces.toMap), defaultHistory(), chess.variant.Standard)

  def makeBoard(str: String, variant: Variant) =
    (Visual << str).withVariant(variant)

  def makeBoard: Board = Board.init(chess.variant.Standard)

  def makeChess960Board(position: Int) = Board(BBoard.fromMap(Chess960.pieces(position)), Chess960)
  def makeChess960Game(position: Int)  = Game(makeChess960Board(position))
  def chess960Boards                   = (0 to 959).map(makeChess960Board).toList

  def makeEmptyBoard: Board = Board(BBoard.empty, chess.variant.Standard)

  def makeGame: Game = Game(makeBoard, White)

  def sortPoss(poss: Seq[Square]): Seq[Square] = poss.sortBy(_.key)

  def pieceMoves(piece: Piece, square: Square): Option[List[Square]] =
    makeEmptyBoard.place(piece, square).map { b =>
      Situation(b, piece.color).movesAt(square).map(_.dest)
    }

  def defaultHistory(
      lastMove: Option[Uci] = None,
      positionHashes: PositionHash = PositionHash.empty,
      castles: Castles = Castles.init,
      checkCount: CheckCount = CheckCount(0, 0),
      unmovedRooks: UnmovedRooks = UnmovedRooks.corners,
      halfMoveClock: HalfMoveClock = HalfMoveClock.initial
  ) = History(
    lastMove = lastMove,
    positionHashes = positionHashes,
    castles = castles,
    checkCount = checkCount,
    unmovedRooks = unmovedRooks,
    halfMoveClock = halfMoveClock
  )

trait MunitExtensions extends munit.FunSuite:
  import alleycats.Zero
  import munit.Location

  def assertNot(cond: => Boolean, clue: => Any = "assertion failed")(using Location): Unit =
    assert(!cond, clue)

  def assertMatch[A](a: A)(f: PartialFunction[A, Unit])(using Location) =
    f.lift(a).getOrElse(fail(s"$a does not match expectations"))

  def assertCloseTo[T](a: T, b: T, delta: Double)(using n: Numeric[T])(using Location) =
    assert(isCloseTo(a, b, delta), s"$a is not close to $b by $delta")

  def assertBetween[T](v: T, min: T, max: T)(using n: Numeric[T])(using Location) =
    assert(n.gteq(v, min))
    assert(n.lteq(v, max))

  private def isCloseTo[T](a: T, b: T, delta: Double)(using n: Numeric[T])(using Location) =
    (n.toDouble(a) - n.toDouble(b)).abs <= delta

  given [A, B](using sr: SameRuntime[B, A]): munit.Compare[A, B] with
    def isEqual(obtained: A, expected: B): Boolean = obtained == sr(expected)

  given [A, B](using comp: munit.Compare[A, B]): munit.Compare[Option[A], Option[B]] with
    def isEqual(obtained: Option[A], expected: Option[B]): Boolean = (obtained, expected) match
      case (Some(o), Some(e)) => comp.isEqual(o, e)
      case (None, None)       => true
      case _                  => false

  given [A, B](using sr: SameRuntime[B, A]): munit.Compare[List[A], List[B]] with
    def isEqual(obtained: List[A], expected: List[B]): Boolean =
      obtained.sameElements(expected.map(sr(_)))

  extension [A](a: A)
    def matchZero[B: Zero](f: PartialFunction[A, B])(using Location): B =
      f.lift(a) | Zero[B].zero

  extension [A](v: Option[A])
    def assertSome(f: PartialFunction[A, Unit])(using Location): Any = v match
      case Some(a) => f.lift(a).getOrElse(fail(s"Unexpected Some value: $a"))
      case None    => fail(s"Expected Some but received None")

  extension [E, A](v: Either[E, A])
    def assertRight(f: PartialFunction[A, Unit])(using Location): Any = v match
      case Right(r) => f.lift(r).getOrElse(fail(s"Unexpected Right value: $r"))
      case Left(e)  => fail(s"Expected Right but received $v")
    def get: A = v match
      case Right(r) => r
      case Left(e)  => fail(s"Expected Right but received $v")

trait ChessTest extends munit.FunSuite with ChessTestCommon with MunitExtensions:
  import munit.Location

  object clockConv:
    given Conversion[Int, Clock.LimitSeconds]     = Clock.LimitSeconds(_)
    given Conversion[Int, Clock.IncrementSeconds] = Clock.IncrementSeconds(_)

  object compare:
    given dests: munit.Compare[Option[List[Square]], Set[Square]] with
      def isEqual(obtained: Option[List[Square]], expected: Set[Square]): Boolean =
        obtained.fold(Set.empty)(_.toSet) == expected

  def fenToGame(positionString: FullFen, variant: Variant)(using Location): Game =
    fenToGameEither(positionString, variant).get

  def visualDests(board: Board, p: Iterable[Square]): String =
    Visual.addNewLines(Visual.>>|(board, Map(p -> 'x')))
  def visualDests(board: Board, p: Option[Iterable[Square]]): String = visualDests(board, p | Nil)

  def assertGame(game: Game, visual: String)(using Location) =
    assertEquals(game.board.visual, (Visual << visual).visual)
