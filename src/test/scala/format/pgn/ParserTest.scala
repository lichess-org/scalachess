package chess
package format.pgn

import Pos._

class ParserTest extends ChessTest {

  import Fixtures._

  "promotion check" in {
    Parser("b8=Q+") must beSuccess
  }

  "nags" in {
    Parser(withNag) must beSuccess
  }

  "lines" in {
    Parser(lines) must beSuccess
  }

  "comments and lines" in {
    Parser(commented) must beSuccess
  }
}
