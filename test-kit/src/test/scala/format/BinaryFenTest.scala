package chess
package format

import chess.variant.{ Standard, Variant }

class BinaryFenTest extends ChessTest:
  test("roundtrip"):
    assertRoundtrip(Standard, "8/8/8/8/8/8/8/8 w - - 0 1")
    assertRoundtrip(Standard, "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")

  private def assertRoundtrip(variant: Variant, fen: String) =
    val situation    = Fen.readWithMoveNumber(variant, EpdFen(fen)).get
    val bytes        = BinaryFen.write(situation)
    val roundtripped = BinaryFen.read(bytes)
    assertEquals(Fen.write(roundtripped), fen)
