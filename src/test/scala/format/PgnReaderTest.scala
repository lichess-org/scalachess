package chess
package format

import Pos._

class PgnReaderTest extends ChessTest {

  import PgnFixtures._

  "only raw moves" should {
    "many games" in {
      forall(raws) { (c: String) ⇒
        PgnReader(c) must beSuccess.like {
          case replay ⇒ replay.moves must have size (c.split(' ').size)
        }
      }
    }
    "example from prod 1" in {
      PgnReader(fromProd1) must beSuccess
    }
    "example from prod 2" in {
      PgnReader(fromProd2) must beSuccess
    }
    "rook promotion" in {
      PgnReader(promoteRook) must beSuccess
    }
    "castle check O-O-O+" in {
      PgnReader(castleCheck1) must beSuccess
    }
    "castle checkmate O-O#" in {
      PgnReader(castleCheck2) must beSuccess
    }
  }
  "tags and moves" should {
    "chess960" in {
      PgnReader(complete960) must beSuccess
    }
    "example from wikipedia" in {
      PgnReader(fromWikipedia) must beSuccess
    }
    "example from chessgames.com" in {
      PgnReader(fromChessgames) must beSuccess
    }
  }
}
