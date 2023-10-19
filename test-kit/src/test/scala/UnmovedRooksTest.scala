package chess

import scala.language.implicitConversions
import Square.*
import variant.Chess960
import format.EpdFen
import chess.format.Fen
import cats.syntax.all.*
import bitboard.Board as BBoard
import bitboard.Bitboard

class UnmovedRooksTest extends ChessTest:

  given munit.Compare[UnmovedRooks, Bitboard] with
    def isEqual(obtained: UnmovedRooks, expected: Bitboard): Boolean =
      obtained.value == expected.value
  given munit.Compare[UnmovedRooks, Long] with
    def isEqual(obtained: UnmovedRooks, expected: Long): Boolean =
      obtained.value == expected

  test("UnmovedRooks with 960 initial fen"):
    assertEquals(
      Fen
        .read(Chess960, EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
        .map(_.board.history.unmovedRooks),
      Option(360287970189639685L)
    )

  test("At the start, unmovedRooks == rooks"):
    chess960Boards.map: board =>
      assertEquals(board.history.unmovedRooks, BBoard.fromMap(board.pieces).rooks)

  test("side 1"):
    Fen
      .read(Chess960, EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
      .map(_.board.history.unmovedRooks)
      .assertSome: ur =>
        assertEquals(ur.side(A1).flatten, Some(QueenSide))
        assertEquals(ur.side(C1).flatten, Some(KingSide))
        assertEquals(ur.side(A8).flatten, Some(QueenSide))
        assertEquals(ur.side(C8).flatten, Some(KingSide))

  chess960Boards.mapWithIndex: (board, n) =>
    test(s"unmovedRooks at position number: $n"):
      board.rooks.squares
        .traverse: square =>
          board.history.unmovedRooks.side(square).flatten
        .assertSome: sides =>
          assertEquals(sides.count(_ == QueenSide), 2)
          assertEquals(sides.count(_ == KingSide), 2)

  test("rook capture rook"):
    fenToGame(
      EpdFen("1r2qkr1/p1b1pppp/3n2n1/2p5/3B4/4PPN1/P4P1P/1RN1QKR1 w KQkq - 0 11"),
      Chess960
    ).playMoves(B1 -> B8)
      .assertRight: g =>
        assertNot(g.board.history.unmovedRooks.contains(B1))
        assertNot(g.board.history.unmovedRooks.contains(B8))
        assert(g.board.history.unmovedRooks.contains(G1))
        assert(g.board.history.unmovedRooks.contains(G8))
        assertEquals(g.board.castles, Castles(true, false, true, false))

  test("capture at the corner"):
    fenToGame(
      EpdFen("1r2k2b/p1qpp2p/1p1nn1r1/2pP4/8/1P1Q2P1/P1P1NP1P/BR2KN1R b Qq - 0 11"),
      Chess960
    ).playMoves(H8 -> A1)
      .assertRight: g =>
        assert(g.board.history.unmovedRooks.contains(B1))
        assert(g.board.history.unmovedRooks.contains(B8))
        assertEquals(g.board.castles, Castles(false, true, false, true))

  test("capture an unmovedRook"):
    fenToGame(
      EpdFen("brnqknr1/pppppp1p/6p1/8/3b1P1P/1P6/P1PPP1P1/BRNQKNRB b KQkq - 0 3"),
      Chess960
    ).playMoves(D4 -> G1)
      .assertRight: g =>
        assert(g.board.history.unmovedRooks.contains(B1))
        assert(g.board.history.unmovedRooks.contains(B8))
        assertNot(g.board.history.unmovedRooks.contains(G1))
        assert(g.board.history.unmovedRooks.contains(G8))
        assertEquals(g.board.castles, Castles(false, true, true, true))

  test("An unmovedRooks moves, white"):
    fenToGame(EpdFen("qrnbkrbn/ppppp1pp/8/5p2/5P2/8/PPPPP1PP/QRNBKRBN w KQkq - 0 2"), Chess960)
      .playMoves(F1 -> F2)
      .assertRight: g =>
        assert(g.board.history.unmovedRooks.contains(B1))
        assert(g.board.history.unmovedRooks.contains(B8))
        assertNot(g.board.history.unmovedRooks.contains(F1))
        assert(g.board.history.unmovedRooks.contains(F8))
        assertEquals(g.board.castles, Castles(false, true, true, true))
  test("An unmovedRooks moves, black"):
    fenToGame(EpdFen("qrnbkrbn/ppppp1pp/8/5p2/4PP2/8/PPPP2PP/QRNBKRBN b KQkq - 0 2"), Chess960)
      .playMoves(F8 -> F6)
      .assertRight: g =>
        assert(g.board.history.unmovedRooks.contains(B1))
        assert(g.board.history.unmovedRooks.contains(B8))
        assert(g.board.history.unmovedRooks.contains(F1))
        assertNot(g.board.history.unmovedRooks.contains(F8))
        assertEquals(g.board.castles, Castles(true, true, false, true))

  test("the king moves, white"):
    fenToGame(EpdFen("rkrnnqbb/p1pppppp/1p6/8/8/1P6/P1PPPPPP/RKRNNQBB w KQkq - 0 2"), Chess960)
      .playMoves(B1 -> B2)
      .assertRight: g =>
        assertNot(g.board.history.unmovedRooks.contains(A1))
        assert(g.board.history.unmovedRooks.contains(A8))
        assertNot(g.board.history.unmovedRooks.contains(C1))
        assert(g.board.history.unmovedRooks.contains(C8))
        assertEquals(g.board.castles, Castles(false, false, true, true))
  test("the king moves, black"):
    fenToGame(
      EpdFen("rkrnnqbb/p1pppppp/1p6/8/5P2/1P6/P1PPP1PP/RKRNNQBB b KQkq - 0 2"),
      Chess960
    )
      .playMoves(B8 -> B7)
      .assertRight: g =>
        assert(g.board.history.unmovedRooks.contains(A1))
        assertNot(g.board.history.unmovedRooks.contains(A8))
        assert(g.board.history.unmovedRooks.contains(C1))
        assertNot(g.board.history.unmovedRooks.contains(C8))
        assertEquals(g.board.castles, Castles(true, true, false, false))
