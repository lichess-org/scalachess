package chess

class ColorTest extends ChessTest:

  "Color" should {
    "unary !" in {
      "white" in { !White must_== Black }
      "black" in { !Black must_== White }
    }

    "passablePawnRank" in {
      "white" in { White.passablePawnRank == Rank.Fifth }
      "black" in { Black.passablePawnRank == Rank.Fourth }
    }
  }
