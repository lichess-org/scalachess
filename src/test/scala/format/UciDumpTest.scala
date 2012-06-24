package chess
package format

import Pos._

class UciDumpTest extends ChessTest {

  import PgnFixtures._

  "only raw moves" should {
    "empty" in {
      UciDump("", None) must beSuccess.like {
        case moves ⇒ moves must_== ""
      }
    }
    "simple" in {
      UciDump(simple, None) must beSuccess.like {
        case moves ⇒ moves must_== "e2e3 b8c6 d2d4 g8f6"
      }
    }
  }
}
