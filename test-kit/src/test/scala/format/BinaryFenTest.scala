package chess
package format

import chess.variant.{ Variant, Standard }

class BinaryFenTest extends ChessTest:
  test("roundtrip"):
    assertRoundtrip(Standard, "8/8/8/8/8/8/8/8 w - - 0 1")

  private def assertRoundtrip(variant: Variant, fen: String) =
    val situation = Fen.readWithMoveNumber(variant, EpdFen(fen)).get
    val roundtripped = BinaryFen.read(BinaryFen.write(situation))
    assertEquals(Fen.write(roundtripped), fen)
