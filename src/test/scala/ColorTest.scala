package chess

class ColorTest extends ChessTest {

  "Color" should {
    "unary !" in {
      "red" in { !Red must_== Black }
      "black" in { !Black must_== Red }
    }
  }
}
