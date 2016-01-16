package chess
package format

import Pos._

class UciMoveTest extends ChessTest {

  import pgn.Fixtures._

  "piotr encoding" should {
    "be reflexive" in {
      val move = Uci.Move("a2g7").get
      Uci.Move piotr move.piotr must_== move.some
    }
  }
}
