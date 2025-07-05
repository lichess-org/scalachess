package chess

import cats.syntax.all.*

import scala.language.implicitConversions

import format.{ FullFen, Fen, Uci, Visual }
import format.pgn.PgnStr
import variant.{ Chess960, Variant, Standard, Crazyhouse }

trait ChessTestCommon:

  given Conversion[String, Position] = Visual.<<
  given Conversion[String, PgnStr]   = PgnStr(_)
  given Conversion[PgnStr, String]   = _.value

  extension (str: String)
    def chess960: Position         = makeBoard(str, chess.variant.Chess960)
    def kingOfTheHill: Position    = makeBoard(str, chess.variant.KingOfTheHill)
    def threeCheck: Position       = makeBoard(str, chess.variant.ThreeCheck)
    def as(color: Color): Position = (Visual << str).withColor(color)

  extension (board: Position)
    def visual                                        = Visual >> board
    def destsFrom(from: Square): Option[List[Square]] =
      board
        .pieceAt(from)
        .map: piece =>
          board.withColor(piece.color).generateMovesAt(from).map(_.dest)

    def seq(actions: Position => Option[Position]*): Option[Position] =
      actions.foldLeft(board.some)(_ flatMap _)

    def place(piece: Piece, at: Square): Option[Position] =
      board.board.put(piece, at).map(board.withBoard)

    def take(at: Square): Option[Position] =
      board.board.take(at).map(board.withBoard)

    def move(orig: Square, dest: Square): Option[Position] =
      board.board.move(orig, dest).map(board.withBoard)

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

  extension (sit: Position)
    def movesAt(s: Square): List[Move] =
      sit.moves.getOrElse(s, Nil)

  def castleHistory(color: Color, kingSide: Boolean, queenSide: Boolean): History =
    val castles = Castles.init.update(color, kingSide, queenSide)
    History(castles = castles, unmovedRooks = UnmovedRooks.corners, crazyData = None)

  def fenToGameEither(positionString: FullFen, variant: Variant): Either[String, Game] =
    Fen
      .read(variant, positionString)
      .map: sit =>
        sit.color -> sit.withVariant(variant)
      .map: (color, board) =>
        Game(variant).copy(position = board.withColor(color))
      .toRight("Could not construct board from Fen")

  def makeBoard(pieces: (Square, Piece)*): Position =
    makeBoard(pieces.toMap, defaultHistory(), chess.variant.Standard, None, None)

  def makeBoard(
      pieces: PieceMap,
      history: History,
      variant: Variant,
      crazyData: Option[Crazyhouse.Data],
      color: Option[Color]
  ): Position =
    new Position(Board.fromMap(pieces), history.copy(crazyData = crazyData), variant, color.getOrElse(White))

  def makeBoard(str: String, variant: Variant) =
    (Visual << str).withVariant(variant)

  def makeChess960Board(position: Int) =
    Position(Board.fromMap(Chess960.initialPieces(position)), Chess960, White)
  def makeChess960Game(position: Int) = Game(makeChess960Board(position))
  def chess960Boards                  = (0 to 959).map(makeChess960Board).toList

  def makeEmptyBoard: Position = Position(Board.empty, Standard, White)

  def makeGame: Game = Game(Standard.initialPosition)

  def sortPoss(poss: Seq[Square]): Seq[Square] = poss.sortBy(_.key)

  def pieceMoves(piece: Piece, square: Square): Option[List[Square]] =
    makeEmptyBoard.place(piece, square).map { b =>
      b.withColor(piece.color).movesAt(square).map(_.dest)
    }

  def defaultHistory(
      lastMove: Option[Uci] = None,
      positionHashes: PositionHash = PositionHash.empty,
      castles: Castles = Castles.init,
      checkCount: CheckCount = CheckCount(0, 0),
      unmovedRooks: UnmovedRooks = UnmovedRooks.corners,
      halfMoveClock: HalfMoveClock = HalfMoveClock.initial,
      crazyData: Option[Crazyhouse.Data] = None
  ) = History(
    lastMove = lastMove,
    positionHashes = positionHashes,
    castles = castles,
    checkCount = checkCount,
    unmovedRooks = unmovedRooks,
    halfMoveClock = halfMoveClock,
    crazyData = crazyData
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

  private def isCloseTo[T](a: T, b: T, delta: Double)(using n: Numeric[T]): Boolean =
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
    def matchZero[B: Zero](f: PartialFunction[A, B]): B =
      f.lift(a) | Zero[B].zero

  extension [A](v: Option[A])
    def assertSome(f: PartialFunction[A, Unit])(using Location): Any = v match
      case Some(a) => f.lift(a).getOrElse(fail(s"Unexpected Some value: $a"))
      case None    => fail(s"Expected Some but received None")

  extension [E, A](v: Either[E, A])
    def assertRight(f: PartialFunction[A, Unit])(using Location): Any = v match
      case Right(r) => f.lift(r).getOrElse(fail(s"Unexpected Right value: $r"))
      case Left(_)  => fail(s"Expected Right but received $v")
    def get: A = v match
      case Right(r) => r
      case Left(_)  => fail(s"Expected Right but received $v")

trait ChessTest extends munit.FunSuite with ChessTestCommon with MunitExtensions:
  import munit.Location

  object clockConv:
    given Conversion[Int, Clock.LimitSeconds]     = Clock.LimitSeconds(_)
    given Conversion[Int, Clock.IncrementSeconds] = Clock.IncrementSeconds(_)

  object compare:
    given dests: munit.Compare[Option[List[Square]], Set[Square]] with
      def isEqual(obtained: Option[List[Square]], expected: Set[Square]): Boolean =
        obtained.fold(Set.empty)(_.toSet) == expected

  def fenToGame(positionString: FullFen, variant: Variant): Game =
    fenToGameEither(positionString, variant).get

  def visualDests(board: Position, p: Iterable[Square]): String =
    Visual.addNewLines(Visual.>>|(board, Map(p -> 'x')))
  def visualDests(board: Position, p: Option[Iterable[Square]]): String = visualDests(board, p | Nil)

  def assertGame(game: Game, visual: String)(using Location) =
    assertEquals(game.position.visual, (Visual << visual).visual)
