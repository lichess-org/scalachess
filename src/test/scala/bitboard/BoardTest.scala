package chess
package bitboard

import cats.syntax.all.*
import munit.FunSuite

import Pos.*
import Helpers.*
import Bitboard.*
import chess.variant.Standard
import chess.format.Fen
import chess.format.EpdFen

class BoardTest extends FunSuite:

  import scala.language.implicitConversions
  given Conversion[Pos, Int] = _.value

  def parseFen(fen: EpdFen): Board =
    Fen.read(fen).map(_.board.board).getOrElse(throw RuntimeException("boooo"))

  test("sliderBlockers") {
    for
      fen <- FenFixtures.fens
      situation = Fen.read(fen).getOrElse(throw RuntimeException("boooo"))
      color <- Color.all
      result   = situation.sliderBlockers
      king     = situation.ourKings.head
      expected = situation.cBoard.sliderBlockers(king)
    yield assertEquals(result, expected.bb)

  }

  test("attacksTo") {
    for
      str <- FenFixtures.fens
      fen  = Fen.read(str).getOrElse(throw RuntimeException("boooo"))
      king = fen.ourKings.head
      i <- 0 to 63
      sq = Pos.at(i).get
      color <- List(Color.White, Color.Black)
      result   = fen.board.attackers(sq, color)
      expected = fen.cBoard.attacksTo(sq, color.white)
    yield assertEquals(result, expected.bb)
  }

  test("discard with standard board") {
    val squaresToDiscards = List.range(0, 16) ++ List.range(48, 64)
    val result            = squaresToDiscards.foldRight(Board.standard)((s, b) => b.discard(Pos.at(s).get))
    assertEquals(result, Board.empty)
  }

  test("discard with test fixtures") {
    FenFixtures.fens.foreach { str =>
      val board  = parseFen(str)
      val result = List.range(0, 64).foldRight(board)((s, b) => b.discard(Pos.at(s).get))
      assertEquals(result, Board.empty)
    }
  }

  test("put a piece into a not empty pos should return none") {
    val board    = Board.standard
    val posToPut = List.range(0, 16) ++ List.range(48, 64)
    val piece    = Piece(White, King)
    val result   = posToPut.map(Pos.at(_).get).map(board.put(piece, _))
    result.foreach(assertEquals(_, None))
  }

  test("put a piece into an empty pos should return new board") {
    val board                       = Board.standard
    val posToPut                    = List.range(17, 47).map(Pos.at(_).get)
    val piece                       = Piece(White, King)
    val result: List[Option[Board]] = posToPut.map(board.put(piece, _))
    result.foreach(x => assertEquals(x.isDefined, true))
  }

  test("putOrReplace with standard board") {
    val board  = Board.standard
    val pieces = board.pieceMap
    val result = pieces.foldRight(Board.empty)((s, b) => b.putOrReplace(s._1, s._2))
    assertEquals(result, board)
  }

  test("putOrReplace with test fixtures") {
    FenFixtures.fens.foreach { str =>
      val board                      = parseFen(str)
      val ss                         = List.range(0, 64).map(Pos.at(_).get)
      val pieces: List[(Pos, Piece)] = ss.mapFilter(s => board.pieceAt(s).map((s, _)))
      val result                     = pieces.foldRight(Board.empty)((s, b) => b.putOrReplace(s._1, s._2))
      assertEquals(result, board)
    }
  }

  test("putOrReplace case 1") {
    val board =
      Fen.read(EpdFen("8/8/8/8/p7/1P6/8/8 w - - 0 1")).getOrElse(throw RuntimeException("booo")).board.board
    val result      = board.putOrReplace(Pos.A4, Piece(White, Pawn))
    val expectedMap = board.pieceMap + (Pos.A4 -> Piece(White, Pawn))
    assertEquals(result.pieceMap, expectedMap)
  }

  test("pieceMap . fromMap === identity") {
    FenFixtures.fens.foreach { str =>
      val board  = Fen.read(str).getOrElse(throw RuntimeException("boooo")).board.board
      val result = Board.fromMap(board.pieceMap)
      assertEquals(result, board)
    }
  }

  test("pieces") {
    val map = Map(A2 -> White.pawn, A3 -> White.rook)
    assertEquals(Board.fromMap(map).pieces.toSet, List(White.pawn, White.rook).toSet)
  }

  test("piecesOf") {
    val map = Map(A2 -> White.pawn, A3 -> White.rook)
    assertEquals(Board.fromMap(map).piecesOf(White), map)
  }

  test("hasPiece") {
    val map = Map(A2 -> White.pawn, A3 -> White.rook)
    assertEquals(Board.fromMap(map).hasPiece(White.pawn), true)
    assertEquals(Board.fromMap(map).hasPiece(White.rook), true)
  }

  test("occupation") {
    val map = Map(A2 -> White.pawn, A3 -> White.rook)
    assertEquals(Board.fromMap(map).occupation, Color.Map(map.keys.toSet, Set()))
  }

  test("hasPiece at pos") {
    val map = Map(A2 -> White.pawn, A3 -> White.rook)
    assertEquals(Board.fromMap(map).hasPiece(A2), true)
    assertEquals(Board.fromMap(map).hasPiece(A3), true)
  }
