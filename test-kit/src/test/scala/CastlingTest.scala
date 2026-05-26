package chess

import chess.format.Fen
import chess.variant.Standard

import scala.language.implicitConversions

import Square.*

class CastlingTest extends ChessTest:

  import compare.dests

  val board: Position = """R   K  R"""

  test("threat on king prevents castling: by a rook"):
    assertEquals(
      board.place(Black.rook, E3).flatMap(_.destsFrom(E1)),
      Set(D1, D2, F2, F1)
    )

  test("threat on king prevents castling: by a knight"):
    assertEquals(board.place(Black.knight, D3).flatMap(_.destsFrom(E1)), Set(D1, D2, E2, F1))

  test("threat on king prevents castling: by a bishop"):
    assertEquals(board.place(Black.bishop, A5).flatMap(_.destsFrom(E1)), Set(D1, E2, F2, F1))

  test("threat on castle trip prevents castling: king side"):
    val board: Position = """R  QK  R"""
    assertEquals(board.place(Black.rook, F3).flatMap(_.destsFrom(E1)), Set(D2, E2))
    assertEquals(board.place(Black.rook, G3).flatMap(_.destsFrom(E1)), Set(D2, E2, F2, F1))

  test("threat on castle trip prevents castling: queen side"):
    val board: Position = """R   KB R"""
    assertEquals(board.place(Black.rook, D3).flatMap(_.destsFrom(E1)), Set(E2, F2))
    assertEquals(board.place(Black.rook, C3).flatMap(_.destsFrom(E1)), Set(D1, D2, E2, F2))

  test("threat on castle trip prevents castling: chess 960"):
    val board: Position = """BK     R"""
    assertEquals(board.place(Black.rook, F3).flatMap(_.destsFrom(B1)), Set(A2, B2, C2, C1))
    assertEquals(board.place(Black.king, E2).flatMap(_.destsFrom(B1)), Set(A2, B2, C2, C1))

  test("threat on rook does not prevent castling king side"):
    val board: Position = """R  QK  R"""
    assertEquals(board.place(Black.rook, H3).flatMap(_.destsFrom(E1)), Set(D2, E2, F1, F2, G1, H1))

  test("threat on rook does not prevent castling queen side"):
    val board: Position = """R   KB R"""
    assertEquals(board.place(Black.rook, A3).flatMap(_.destsFrom(E1)), Set(A1, C1, D1, D2, E2, F2))

  test("threat on rooks trip does not prevent castling queen side"):
    val board: Position = """R   KB R"""
    assertEquals(board.place(Black.rook, B3).flatMap(_.destsFrom(E1)), Set(A1, C1, D1, D2, E2, F2))

  test("only rooks listed in castlingRights are recognised as castle-eligible"):
    val s1 = Fen.read(Standard, Fen.Full("rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w Qq - 0 1")).get
    // FEN said `Qq`: white queen-side and black queen-side. Other sides are not in castlingRights.
    assert(s1.castlingRights.contains(Square.A1))
    assertNot(s1.castlingRights.contains(Square.H1))
    assert(s1.castlingRights.contains(Square.A8))
    assertNot(s1.castlingRights.contains(Square.H8))
    // (Path-clearing is independent: the FEN's pieces block the actual moves; that's not our concern here.)
