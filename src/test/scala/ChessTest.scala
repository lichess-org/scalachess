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
import bitboard.Bitboard
import bitboard.Bitboard.bitboard
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

  extension (ps: List[Pos]) def bb: Bitboard = ps.foldLeft(Bitboard.empty)((bb, pos) => bb | pos.bitboard)

  extension (board: Board) def visual = Visual >> board

  extension (game: Game)
    def as(color: Color): Game = game.withPlayer(color)

    def playMoves(moves: (Pos, Pos)*): Validated[String, Game] = playMoveList(moves)

    def playMoveList(moves: Iterable[(Pos, Pos)]): Validated[String, Game] =
      val vg = moves.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, move) =>
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
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None
    ): Validated[String, Game] =
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

  def makeBoard(pieces: (Pos, Piece)*): Board =
    Board(BBoard.fromMap(pieces.toMap), defaultHistory(), chess.variant.Standard)

  def makeBoard(str: String, variant: Variant) =
    Visual << str withVariant variant

  def makeBoard: Board = Board init chess.variant.Standard

  def makeChess960Board(position: Int) = Board(Chess960.pieces(position), Chess960.castles, Chess960)
  def makeChess960Game(position: Int)  = Game(makeChess960Board(position))
  def chess960Boards                   = (0 to 959).map(makeChess960Board).toList

  def makeEmptyBoard: Board = Board empty chess.variant.Standard

  def bePoss(poss: Pos*) = // : Matcher[Option[Iterable[Pos]]] =
    beSome { (p: Iterable[Pos]) =>
      sortPoss(p.toList).map(_.key) must_== sortPoss(poss.toList).map(_.key)
    }

  def makeGame: Game = Game(makeBoard, White)

  def bePoss(board: Board, visual: String) = // : Matcher[Option[Iterable[Pos]]] =
    beSome { (p: Iterable[Pos]) =>
      Visual.addNewLines(Visual.>>|(board, Map(p -> 'x'))) must_== visual
    }

  def beBoard(visual: String): Matcher[Validated[String, Board]] =
    beValid.like { case b =>
      b.visual must_== (Visual << visual).visual
    }

  def beSituation(visual: String): Matcher[Validated[String, Situation]] =
    beValid.like { case s =>
      s.board.visual must_== (Visual << visual).visual
    }

  def beGame(visual: String): Matcher[Validated[String, Game]] =
    beValid.like { case g =>
      g.board.visual must_== (Visual << visual).visual
    }

  def sortPoss(poss: Seq[Pos]): Seq[Pos] = poss.sortBy(_.key)

  def pieceMoves(piece: Piece, pos: Pos): Option[List[Pos]] =
    (makeEmptyBoard place (piece, pos)) map { b =>
      Situation(b, piece.color).movesAt(pos).map(_.dest)
    }

  def defaultHistory(
      lastMove: Option[Uci] = None,
      positionHashes: PositionHash = Monoid[PositionHash].empty,
      castles: Castles = Castles.all,
      checkCount: CheckCount = CheckCount(0, 0),
      unmovedRooks: UnmovedRooks = UnmovedRooks.corners,
      halfMoveClock: HalfMoveClock = HalfMoveClock(0)
  ) = History(
    lastMove = lastMove,
    positionHashes = positionHashes,
    castles = castles,
    checkCount = checkCount,
    unmovedRooks = unmovedRooks,
    halfMoveClock = halfMoveClock
  )
