package chess

class PieceTest extends ChessTest:

  test("compare objects and - method"):
    assertNotEquals(White - Pawn, Black - Pawn)
  test("compare value and - method"):
    val color = White
    assertNotEquals(color - Pawn, Black - Pawn)
