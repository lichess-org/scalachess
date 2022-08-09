package chess

class PieceTest extends ChessTest {

  "Piece" should {
    "compare" in {
      "objects and - method" in {
        !Red - Pawn must_== Black - Pawn
      }
      "value and - method" in {
        val color = Red
        !color - Pawn must_== Black - Pawn
      }
    }
  }
}
