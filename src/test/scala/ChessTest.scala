package chess

import cats.syntax.all.*
import org.specs2.matcher.{ EitherMatchers, Matcher }
import org.specs2.mutable.Specification
import scala.language.implicitConversions

import chess.format.{ EpdFen, Fen, Visual }
import chess.format.pgn.PgnStr
import chess.variant.Variant
import bitboard.Board as BBoard
import cats.kernel.Monoid
import chess.format.Uci
import chess.variant.Chess960

trait ChessTest extends Specification with EitherMatchers:

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

  def fenToGame(positionString: EpdFen, variant: Variant) =
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
    Visual << str withVariant variant

  def makeBoard: Board = Board init chess.variant.Standard

  def makeChess960Board(position: Int) = Board(Chess960.pieces(position), Chess960.castles, Chess960)
  def makeChess960Game(position: Int)  = Game(makeChess960Board(position))
  def chess960Boards                   = (0 to 959).map(makeChess960Board).toList

  def makeEmptyBoard: Board = Board empty chess.variant.Standard

  def bePoss(poss: Square*) = // : Matcher[Option[Iterable[square]]] =
    beSome { (p: Iterable[Square]) =>
      sortPoss(p.toList).map(_.key) must_== sortPoss(poss.toList).map(_.key)
    }

  def makeGame: Game = Game(makeBoard, White)

  def bePoss(board: Board, visual: String) = // : Matcher[Option[Iterable[square]]] =
    beSome { (p: Iterable[Square]) =>
      Visual.addNewLines(Visual.>>|(board, Map(p -> 'x'))) must_== visual
    }

  def beBoard(visual: String): Matcher[Either[ErrorStr, Board]] =
    beRight.like { case b =>
      b.visual must_== (Visual << visual).visual
    }

  def beSituation(visual: String): Matcher[Either[ErrorStr, Situation]] =
    beRight.like { case s =>
      s.board.visual must_== (Visual << visual).visual
    }

  def beGame(visual: String): Matcher[Either[ErrorStr, Game]] =
    beRight.like { case g =>
      g.board.visual must_== (Visual << visual).visual
    }

  def sortPoss(poss: Seq[Square]): Seq[Square] = poss.sortBy(_.key)

  def pieceMoves(piece: Piece, square: Square): Option[List[Square]] =
    (makeEmptyBoard place (piece, square)) map { b =>
      Situation(b, piece.color).movesAt(square).map(_.dest)
    }

  def defaultHistory(
      lastMove: Option[Uci] = None,
      positionHashes: PositionHash = Monoid[PositionHash].empty,
      castles: Castles = Castles.all,
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
