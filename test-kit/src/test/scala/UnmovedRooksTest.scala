package chess

import cats.syntax.all.*
import chess.format.Fen

import scala.language.implicitConversions

import Square.*
import variant.{ Atomic, Chess960 }
import format.FullFen

class UnmovedRooksTest extends ChessTest:

  given munit.Compare[CastlingRights, Bitboard] with
    def isEqual(obtained: CastlingRights, expected: Bitboard): Boolean =
      obtained.value == expected.value
  given munit.Compare[CastlingRights, Long] with
    def isEqual(obtained: CastlingRights, expected: Long): Boolean =
      obtained.value == expected

  /** Helper: assert that a position's per-side castling availability matches the four booleans. */
  def assertCanCastle(
      position: Position,
      whiteKingSide: Boolean,
      whiteQueenSide: Boolean,
      blackKingSide: Boolean,
      blackQueenSide: Boolean
  )(using munit.Location): Unit =
    assertEquals(position.canCastle(White, KingSide), whiteKingSide, "White king-side")
    assertEquals(position.canCastle(White, QueenSide), whiteQueenSide, "White queen-side")
    assertEquals(position.canCastle(Black, KingSide), blackKingSide, "Black king-side")
    assertEquals(position.canCastle(Black, QueenSide), blackQueenSide, "Black queen-side")

  test("castlingRights with 960 initial fen"):
    assertEquals(
      Fen
        .read(Chess960, FullFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
        .map(_.history.castlingRights),
      Option(CastlingRights(360287970189639685L))
    )

  test("At the start, castlingRights == rooks"):
    chess960Boards.map: board =>
      assertEquals(board.history.castlingRights, Board.fromMap(board.pieces).rooks)

  test("At the start, both sides should have two unmoved rooks classified per side"):
    chess960Boards.map: board =>
      board.rooks.squares
        .traverse: square =>
          board.castlingSide(square)
        .assertSome: sides =>
          assertEquals(sides.count(_ == QueenSide), 2)
          assertEquals(sides.count(_ == KingSide), 2)

  test("side 1"):
    Fen
      .read(Chess960, FullFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
      .assertSome: position =>
        assertEquals(position.castlingSide(A1), Some(QueenSide))
        assertEquals(position.castlingSide(C1), Some(KingSide))
        assertEquals(position.castlingSide(A8), Some(QueenSide))
        assertEquals(position.castlingSide(C8), Some(KingSide))

  test("rook capture rook"):
    fenToGame(
      FullFen("1r2qkr1/p1b1pppp/3n2n1/2p5/3B4/4PPN1/P4P1P/1RN1QKR1 w KQkq - 0 11"),
      Chess960
    ).playMoves(B1 -> B8)
      .assertRight: g =>
        assertNot(g.history.castlingRights.contains(B1))
        assertNot(g.history.castlingRights.contains(B8))
        assert(g.history.castlingRights.contains(G1))
        assert(g.history.castlingRights.contains(G8))
        assertCanCastle(
          g.position,
          whiteKingSide = true,
          whiteQueenSide = false,
          blackKingSide = true,
          blackQueenSide = false
        )

  test("capture at the corner"):
    fenToGame(
      FullFen("1r2k2b/p1qpp2p/1p1nn1r1/2pP4/8/1P1Q2P1/P1P1NP1P/BR2KN1R b Qq - 0 11"),
      Chess960
    ).playMoves(H8 -> A1)
      .assertRight: g =>
        assert(g.history.castlingRights.contains(B1))
        assert(g.history.castlingRights.contains(B8))
        assertCanCastle(
          g.position,
          whiteKingSide = false,
          whiteQueenSide = true,
          blackKingSide = false,
          blackQueenSide = true
        )

  test("capture an unmovedRook"):
    fenToGame(
      FullFen("brnqknr1/pppppp1p/6p1/8/3b1P1P/1P6/P1PPP1P1/BRNQKNRB b KQkq - 0 3"),
      Chess960
    ).playMoves(D4 -> G1)
      .assertRight: g =>
        assert(g.history.castlingRights.contains(B1))
        assert(g.history.castlingRights.contains(B8))
        assertNot(g.history.castlingRights.contains(G1))
        assert(g.history.castlingRights.contains(G8))
        assertCanCastle(
          g.position,
          whiteKingSide = false,
          whiteQueenSide = true,
          blackKingSide = true,
          blackQueenSide = true
        )

  test("Atomic: explode an unmovedRook"):
    fenToGame(
      FullFen("rnbqk1nr/p1p3pp/4pp1B/1p1p4/3PP3/b1N5/PPP2PPP/R2QKBNR w KQkq - 2 6"),
      Atomic
    ).playMoves(H6 -> G7)
      .assertRight: g =>
        assert(g.history.castlingRights.contains(A1))
        assert(g.history.castlingRights.contains(H1))
        assert(g.history.castlingRights.contains(A8))
        assertNot(g.history.castlingRights.contains(H8))
        assertCanCastle(
          g.position,
          whiteKingSide = true,
          whiteQueenSide = true,
          blackKingSide = false,
          blackQueenSide = true
        )

  test("An unmovedRooks moves, white"):
    fenToGame(FullFen("qrnbkrbn/ppppp1pp/8/5p2/5P2/8/PPPPP1PP/QRNBKRBN w KQkq - 0 2"), Chess960)
      .playMoves(F1 -> F2)
      .assertRight: g =>
        assert(g.history.castlingRights.contains(B1))
        assert(g.history.castlingRights.contains(B8))
        assertNot(g.history.castlingRights.contains(F1))
        assert(g.history.castlingRights.contains(F8))
        assertCanCastle(
          g.position,
          whiteKingSide = false,
          whiteQueenSide = true,
          blackKingSide = true,
          blackQueenSide = true
        )

  test("An unmovedRooks moves, black"):
    fenToGame(FullFen("qrnbkrbn/ppppp1pp/8/5p2/4PP2/8/PPPP2PP/QRNBKRBN b KQkq - 0 2"), Chess960)
      .playMoves(F8 -> F6)
      .assertRight: g =>
        assert(g.history.castlingRights.contains(B1))
        assert(g.history.castlingRights.contains(B8))
        assert(g.history.castlingRights.contains(F1))
        assertNot(g.history.castlingRights.contains(F8))
        assertCanCastle(
          g.position,
          whiteKingSide = true,
          whiteQueenSide = true,
          blackKingSide = false,
          blackQueenSide = true
        )

  test("the king moves, white"):
    fenToGame(FullFen("rkrnnqbb/p1pppppp/1p6/8/8/1P6/P1PPPPPP/RKRNNQBB w KQkq - 0 2"), Chess960)
      .playMoves(B1 -> B2)
      .assertRight: g =>
        assertNot(g.history.castlingRights.contains(A1))
        assert(g.history.castlingRights.contains(A8))
        assertNot(g.history.castlingRights.contains(C1))
        assert(g.history.castlingRights.contains(C8))
        assertCanCastle(
          g.position,
          whiteKingSide = false,
          whiteQueenSide = false,
          blackKingSide = true,
          blackQueenSide = true
        )

  test("the king moves, black"):
    fenToGame(
      FullFen("rkrnnqbb/p1pppppp/1p6/8/5P2/1P6/P1PPP1PP/RKRNNQBB b KQkq - 0 2"),
      Chess960
    )
      .playMoves(B8 -> B7)
      .assertRight: g =>
        assert(g.history.castlingRights.contains(A1))
        assertNot(g.history.castlingRights.contains(A8))
        assert(g.history.castlingRights.contains(C1))
        assertNot(g.history.castlingRights.contains(C8))
        assertCanCastle(
          g.position,
          whiteKingSide = true,
          whiteQueenSide = true,
          blackKingSide = false,
          blackQueenSide = false
        )
