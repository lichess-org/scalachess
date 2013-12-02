package chess
package format

import Pos._

class UciDumpTest extends ChessTest {

  import pgn.Fixtures._

  "only raw moves" should {
    "empty" in {
      UciDump(Nil, None, Variant.Standard) must beSuccess.like {
        case x ⇒ x must beEmpty
      }
    }
    "simple" in {
      UciDump(simple, None, Variant.Standard) must beSuccess.like {
        case moves ⇒ moves must_== "e2e3 b8c6 d2d4 g8f6".split(" ").toList
      }
    }
    "complete" in {
      UciDump(fromWikipedia, None, Variant.Standard) must beSuccess.like {
        case moves ⇒ moves must_== "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6 e1g1 f8e7 f1e1 b7b5 a4b3 d7d6 c2c3 e8g8 h2h3 c6b8 d2d4 b8d7 c3c4 c7c6 c4b5 a6b5 b1c3 c8b7 c1g5 b5b4 c3b1 h7h6 g5h4 c6c5 d4e5 f6e4 h4e7 d8e7 e5d6 e7f6 b1d2 e4d6 d2c4 d6c4 b3c4 d7b6 f3e5 a8e8 c4f7 f8f7 e5f7 e8e1 d1e1 g8f7 e1e3 f6g5 e3g5 h6g5 b2b3 f7e6 a2a3 e6d6 a3b4 c5b4 a1a5 b6d5 f2f3 b7c8 g1f2 c8f5 a5a7 g7g6 a7a6 d6c5 f2e1 d5f4 g2g3 f4h3 e1d2 c5b5 a6d6 b5c5 d6a6 h3f2 g3g4 f5d3 a6e6".split(" ").toList
      }
    }
    "960" in {
      UciDump(complete960, None, Variant.Chess960) must beSuccess.like {
        case moves ⇒ moves must_== "e2e3 e8f6 f1g3 f8e6 e1f3 d7d5 f3d4 e6d4 e3d4 e7e6 d1e1 f6g4 e1e2 f7f6 c2c4 d5c4 b1e4 d8d4 e4f3 g4e5 g3e4 e5f3 g2f3 g8f7 e4f6 g7f6 c1d1 e6e5 h2h4 f7g6 g1h2 g6f5 e2e5 f6e5 h2e5 h8e5 h1f1 e5f4 d2d3 d4d3 d1e1 f4d2".split(" ").toList
      }
    }
  }
}
