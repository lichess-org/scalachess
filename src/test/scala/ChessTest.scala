package chess

import cats.data.Validated
import cats.syntax.option.*
import org.specs2.matcher.Matcher
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification
import scala.language.implicitConversions

import chess.format.{ EpdFen, Fen, Visual }
import chess.format.pgn.PgnStr
import chess.variant.Variant
import bitboard.Board as BBoard
import cats.kernel.Monoid
import chess.format.Uci
import chess.variant.Chess960

trait ChessTest extends Specification with ValidatedMatchers:

  given Conversion[String, Board]  = Visual.<<
  given Conversion[String, PgnStr] = PgnStr(_)
  given Conversion[PgnStr, String] = _.value

  extension (color: Color)
    def -(role: Role) = Piece(color, role)
    def pawn          = color - Pawn
    def bishop        = color - Bishop
    def knight        = color - Knight
    def rook          = color - Rook
    def queen         = color - Queen
    def king          = color - King

  extension (str: String)
    def chess960: Board             = makeBoard(str, chess.variant.Chess960)
    def kingOfTheHill: Board        = makeBoard(str, chess.variant.KingOfTheHill)
    def threeCheck: Board           = makeBoard(str, chess.variant.ThreeCheck)
    def as(color: Color): Situation = Situation(Visual << str, color)

  extension (board: Board)
    def visual = Visual >> board
    def destsFrom(from: Square): Option[List[Square]] =
      board(from).map { piece =>
        Situation(board, piece.color).generateMovesAt(from).map(_.dest)
      }

  extension (game: Game)
    def as(color: Color): Game = game.withPlayer(color)

    def playMoves(moves: (Square, Square)*): Validated[ErrorStr, Game] = playMoveList(moves)

    def playMoveList(moves: Iterable[(Square, Square)]): Validated[ErrorStr, Game] =
      val vg = moves.foldLeft(Validated.valid(game): Validated[ErrorStr, Game]) { (vg, move) =>
        // vg foreach { x =>
        // println(s"------------------------ ${x.turns} = $move")
        // }
        // because possible moves are asked for player highlight
        // before the move is played (on initial situation)
        // val _ = vg map { _.situation.destinations }
        val ng = vg flatMap { g =>
          g(move._1, move._2) map (_._1)
        }
        ng
      }
      // vg foreach { x => println("========= PGN: " + x.pgnMoves) }
      vg

    def playMove(
        orig: Square,
        dest: Square,
        promotion: Option[PromotableRole] = None
    ): Validated[ErrorStr, Game] =
      game.apply(orig, dest, promotion) map (_._1)

    def withClock(c: Clock) = game.copy(clock = Option(c))

  def fenToGame(positionString: EpdFen, variant: Variant) =
    val situation = Fen.read(variant, positionString)
    situation map { sit =>
      sit.color -> sit.withVariant(variant).board
    } toValid "Could not construct situation from Fen" map { case (color, board) =>
      Game(variant).copy(
        situation = Situation(board, color)
      )
    }

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

  def beBoard(visual: String): Matcher[Validated[ErrorStr, Board]] =
    beValid.like { case b =>
      b.visual must_== (Visual << visual).visual
    }

  def beSituation(visual: String): Matcher[Validated[ErrorStr, Situation]] =
    beValid.like { case s =>
      s.board.visual must_== (Visual << visual).visual
    }

  def beGame(visual: String): Matcher[Validated[ErrorStr, Game]] =
    beValid.like { case g =>
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
