package chess

import cats.syntax.all.*
import chess.format.Fen

import scala.language.implicitConversions

import Square.*
import variant.{ Atomic, Chess960 }
import format.FullFen
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
        .read(Chess960, FullFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
        .map(_.history.unmovedRooks),
      Option(360287970189639685L)
    )

  test("At the start, unmovedRooks == rooks"):
    chess960Boards.map: board =>
      assertEquals(board.history.unmovedRooks, BBoard.fromMap(board.pieces).rooks)

  test("At the start, both sides should have two unmoved rooks"):
    chess960Boards.map: board =>
      board.rooks.squares
        .traverse: square =>
          board.history.unmovedRooks.side(square).flatten
        .assertSome: sides =>
          assertEquals(sides.count(_ == QueenSide), 2)
          assertEquals(sides.count(_ == KingSide), 2)

  test("side 1"):
    Fen
      .read(Chess960, FullFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
      .map(_.history.unmovedRooks)
      .assertSome: ur =>
        assertEquals(ur.side(A1).flatten, Some(QueenSide))
        assertEquals(ur.side(C1).flatten, Some(KingSide))
        assertEquals(ur.side(A8).flatten, Some(QueenSide))
        assertEquals(ur.side(C8).flatten, Some(KingSide))

  test("rook capture rook"):
    fenToGame(
      FullFen("1r2qkr1/p1b1pppp/3n2n1/2p5/3B4/4PPN1/P4P1P/1RN1QKR1 w KQkq - 0 11"),
      Chess960
    ).playMoves(B1 -> B8)
      .assertRight: g =>
        assertNot(g.history.unmovedRooks.contains(B1))
        assertNot(g.history.unmovedRooks.contains(B8))
        assert(g.history.unmovedRooks.contains(G1))
        assert(g.history.unmovedRooks.contains(G8))
        assertEquals(g.board.castles, Castles(true, false, true, false))

  test("capture at the corner"):
    fenToGame(
      FullFen("1r2k2b/p1qpp2p/1p1nn1r1/2pP4/8/1P1Q2P1/P1P1NP1P/BR2KN1R b Qq - 0 11"),
      Chess960
    ).playMoves(H8 -> A1)
      .assertRight: g =>
        assert(g.history.unmovedRooks.contains(B1))
        assert(g.history.unmovedRooks.contains(B8))
        assertEquals(g.board.castles, Castles(false, true, false, true))

  test("capture an unmovedRook"):
    fenToGame(
      FullFen("brnqknr1/pppppp1p/6p1/8/3b1P1P/1P6/P1PPP1P1/BRNQKNRB b KQkq - 0 3"),
      Chess960
    ).playMoves(D4 -> G1)
      .assertRight: g =>
        assert(g.history.unmovedRooks.contains(B1))
        assert(g.history.unmovedRooks.contains(B8))
        assertNot(g.history.unmovedRooks.contains(G1))
        assert(g.history.unmovedRooks.contains(G8))
        assertEquals(g.board.castles, Castles(false, true, true, true))

  test("Atomic: explode an unmovedRook"):
    fenToGame(
      FullFen("rnbqk1nr/p1p3pp/4pp1B/1p1p4/3PP3/b1N5/PPP2PPP/R2QKBNR w KQkq - 2 6"),
      Atomic
    ).playMoves(H6 -> G7)
      .assertRight: g =>
        assert(g.history.unmovedRooks.contains(A1))
        assert(g.history.unmovedRooks.contains(H1))
        assert(g.history.unmovedRooks.contains(A8))
        assertNot(g.history.unmovedRooks.contains(H8))
        assertEquals(
          g.board.castles,
          Castles(whiteKingSide = true, whiteQueenSide = true, blackKingSide = false, blackQueenSide = true)
        )

  test("An unmovedRooks moves, white"):
    fenToGame(FullFen("qrnbkrbn/ppppp1pp/8/5p2/5P2/8/PPPPP1PP/QRNBKRBN w KQkq - 0 2"), Chess960)
      .playMoves(F1 -> F2)
      .assertRight: g =>
        assert(g.history.unmovedRooks.contains(B1))
        assert(g.history.unmovedRooks.contains(B8))
        assertNot(g.history.unmovedRooks.contains(F1))
        assert(g.history.unmovedRooks.contains(F8))
        assertEquals(g.board.castles, Castles(false, true, true, true))
  test("An unmovedRooks moves, black"):
    fenToGame(FullFen("qrnbkrbn/ppppp1pp/8/5p2/4PP2/8/PPPP2PP/QRNBKRBN b KQkq - 0 2"), Chess960)
      .playMoves(F8 -> F6)
      .assertRight: g =>
        assert(g.history.unmovedRooks.contains(B1))
        assert(g.history.unmovedRooks.contains(B8))
        assert(g.history.unmovedRooks.contains(F1))
        assertNot(g.history.unmovedRooks.contains(F8))
        assertEquals(g.board.castles, Castles(true, true, false, true))

  test("the king moves, white"):
    fenToGame(FullFen("rkrnnqbb/p1pppppp/1p6/8/8/1P6/P1PPPPPP/RKRNNQBB w KQkq - 0 2"), Chess960)
      .playMoves(B1 -> B2)
      .assertRight: g =>
        assertNot(g.history.unmovedRooks.contains(A1))
        assert(g.history.unmovedRooks.contains(A8))
        assertNot(g.history.unmovedRooks.contains(C1))
        assert(g.history.unmovedRooks.contains(C8))
        assertEquals(g.board.castles, Castles(false, false, true, true))
  test("the king moves, black"):
    fenToGame(
      FullFen("rkrnnqbb/p1pppppp/1p6/8/5P2/1P6/P1PPP1PP/RKRNNQBB b KQkq - 0 2"),
      Chess960
    )
      .playMoves(B8 -> B7)
      .assertRight: g =>
        assert(g.history.unmovedRooks.contains(A1))
        assertNot(g.history.unmovedRooks.contains(A8))
        assert(g.history.unmovedRooks.contains(C1))
        assertNot(g.history.unmovedRooks.contains(C8))
        assertEquals(g.board.castles, Castles(true, true, false, false))
