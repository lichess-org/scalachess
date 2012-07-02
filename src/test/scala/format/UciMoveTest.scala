package chess
package format

import Pos._

class UciMoveTest extends ChessTest {

  import pgn.Fixtures._

  "piotr encoding" should {
    "be reflexive" in {
      val move = UciMove("a2g7").get
      UciMove piotr move.piotr must_== move.some
    }
  }
}
