package chess
package format

import Pos._

class UciDumpTest extends ChessTest {

  import pgn.Fixtures._

  "only raw moves" should {
    "empty" in {
      UciDump("", None) must beFailure
    }
    "simple" in {
      UciDump(simple, None) must beSuccess.like {
        case moves ⇒ moves must_== "e2e3 b8c6 d2d4 g8f6"
      }
    }
  }
}
