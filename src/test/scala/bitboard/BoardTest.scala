package chess
package bitboard

import cats.syntax.all.*
import munit.FunSuite

import Pos.*

class BoardTest extends FunSuite:

  import scala.language.implicitConversions
  given Conversion[Pos, Int] = _.value

  import Helpers.*
  import Bitboard.*
  import Fen.*

  test("sliderBlockers") {
    FenFixtures.fens.foreach { str =>
      val fen      = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val result   = fen.board.sliderBlockers(fen.state.turn)
      val king     = fen.ourKings.head
      val expected = fen.cBoard.sliderBlockers(king)
      assertEquals(result, expected.bb)
    }
  }

  test("attacksTo") {
    for
      str <- FenFixtures.fens
      fen  = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      king = fen.ourKings.head
      i <- 0 to 63
      sq = Pos.at(i).get
      color <- List(Color.White, Color.Black)
      result   = fen.board.attacksTo(sq, color)
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
      val fen    = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val result = List.range(0, 64).foldRight(fen.board)((s, b) => b.discard(Pos.at(s).get))
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
      val fen                        = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val ss                         = List.range(0, 64).map(Pos.at(_).get)
      val pieces: List[(Pos, Piece)] = ss.mapFilter(s => fen.board.pieceAt(s).map((s, _)))
      val result                     = pieces.foldRight(Board.empty)((s, b) => b.putOrReplace(s._1, s._2))
      assertEquals(result, fen.board)
    }
  }

  test("putOrReplace case 1") {
    val fen         = Fen.parse("8/8/8/8/p7/1P6/8/8 w - - 0 1").getOrElse(throw RuntimeException("booo"))
    val result      = fen.board.putOrReplace(Pos.A4, Piece(White, Pawn))
    val expectedMap = fen.board.pieceMap + (Pos.A4 -> Piece(White, Pawn))
    assertEquals(result.pieceMap, expectedMap)
  }

  test("play with standard board") {
    val board       = Board.standard
    val newBoard    = board.play(Color.White)(Move.Normal(Pos.E2, Pos.E4, Pawn, false))
    val expectedMap = (board.pieceMap - Pos.E2) + (Pos.E4 -> Piece(White, Pawn))
    val result      = newBoard.pieceMap
    assertEquals(result, expectedMap)
  }

  test("pieceMap . fromMap === identity") {
    FenFixtures.fens.foreach { str =>
      val board  = Fen.parse(str).getOrElse(throw RuntimeException("boooo")).board
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

  test("move - a random case") {
    val fen =
      Fen
        .parse("rnbqkbnr/1ppppppp/8/8/p7/PPP5/3PPPPP/RNBQKBNR w KQkq - 0 4")
        .getOrElse(throw RuntimeException("booo"))
    val result      = fen.play(Move.Normal(Pos.B3, Pos.A4, Pawn, true))
    val expectedMap = (fen.board.pieceMap - Pos.B3) + (Pos.A4 -> Piece(White, Pawn))
    assertEquals(result.board.pieceMap, expectedMap)
  }
