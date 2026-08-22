package chess
package format

import chess.variant.{ Atomic, Chess960, Crazyhouse, Standard, Variant }

import Square.*

class UciDumpTest extends ChessTest:

  private def lastMoveOf(fen: String, variant: Variant)(orig: Square, dest: Square): String =
    val game = fenToGame(FullFen(fen), variant)(orig, dest).get._1
    UciDump.lastMove(game.history.lastMove.get, game.position)

  private val whiteCastles = lastMoveOf("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", Standard)
  private val blackCastles = lastMoveOf("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1", Standard)

  test("castling is shown on the king destination"):
    assertEquals(whiteCastles(E1, G1), "e1g1")
    assertEquals(whiteCastles(E1, H1), "e1g1")
    assertEquals(whiteCastles(E1, C1), "e1c1")
    assertEquals(whiteCastles(E1, A1), "e1c1")
    assertEquals(blackCastles(E8, G8), "e8g8")
    assertEquals(blackCastles(E8, H8), "e8g8")
    assertEquals(blackCastles(E8, C8), "e8c8")
    assertEquals(blackCastles(E8, A8), "e8c8")

  test("chess960 castling is shown on the rook"):
    val castles = lastMoveOf("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", Chess960)
    assertEquals(castles(E1, H1), "e1h1")
    assertEquals(castles(E1, A1), "e1a1")

  test("a move that only looks like castling is shown as is"):
    assertEquals(lastMoveOf("2k5/8/8/8/8/6K1/8/r3Q3 w - - 0 1", Standard)(E1, A1), "e1a1")
    assertEquals(lastMoveOf("2k5/8/8/8/8/6K1/8/4Q2r w - - 0 1", Standard)(E1, H1), "e1h1")
    assertEquals(lastMoveOf("R3q3/8/8/8/8/6k1/8/2K5 b - - 0 1", Standard)(E8, A8), "e8a8")
    assertEquals(lastMoveOf("4r2R/8/8/8/8/6k1/8/2K5 b - - 0 1", Standard)(E8, H8), "e8h8")

  test("atomic castling is shown on the king destination"):
    val whiteAtomic = lastMoveOf("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", Atomic)
    val blackAtomic = lastMoveOf("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1", Atomic)
    assertEquals(whiteAtomic(E1, H1), "e1g1")
    assertEquals(whiteAtomic(E1, A1), "e1c1")
    assertEquals(blackAtomic(E8, H8), "e8g8")
    assertEquals(blackAtomic(E8, A8), "e8c8")

  test("an atomic capture that only looks like castling is shown as is"):
    assertEquals(lastMoveOf("2k5/8/8/8/8/6K1/8/r3Q3 w - - 0 1", Atomic)(E1, A1), "e1a1")
    assertEquals(lastMoveOf("2k5/8/8/8/8/6K1/8/4Q2r w - - 0 1", Atomic)(E1, H1), "e1h1")
    assertEquals(lastMoveOf("R3q3/8/8/8/8/6k1/8/2K5 b - - 0 1", Atomic)(E8, A8), "e8a8")
    assertEquals(lastMoveOf("4r2R/8/8/8/8/6k1/8/2K5 b - - 0 1", Atomic)(E8, H8), "e8h8")

  test("regular moves are shown as is"):
    assertEquals(lastMoveOf("4k3/8/8/8/8/8/8/4K3 w - - 0 1", Standard)(E1, E2), "e1e2")

  test("drops are shown on their square"):
    assertEquals(UciDump.lastMove(Uci.Drop(Pawn, C7), Crazyhouse.initialPosition), "c7c7")
