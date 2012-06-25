package chess
package format

import Pos._

class PgnParserTest extends ChessTest {

  import PgnFixtures._

  "promotion check" in {
    PgnParser("b8=Q+") must beSuccess
  }
}
