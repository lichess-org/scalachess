package chess
package format.pgn

import Pos._

class ReaderTest extends ChessTest {

  import Fixtures._

  "only raw moves" should {
    "many games" in {
      forall(raws) { (c: String) ⇒
        Reader(c) must beSuccess.like {
          case replay ⇒ replay.moves must have size (c.split(' ').size)
        }
      }
    }
    "example from prod 1" in {
      Reader(fromProd1) must beSuccess
    }
    "example from prod 2" in {
      Reader(fromProd2) must beSuccess
    }
    "rook promotion" in {
      Reader(promoteRook) must beSuccess
    }
    "castle check O-O-O+" in {
      Reader(castleCheck1) must beSuccess
    }
    "castle checkmate O-O#" in {
      Reader(castleCheck2) must beSuccess
    }
  }
  "tags and moves" should {
    "chess960" in {
      Reader(complete960) must beSuccess
    }
    "with empty lines" in {
      Reader("\n" + complete960 + "\n") must beSuccess
    }
    "example from wikipedia" in {
      Reader(fromWikipedia) must beSuccess
    }
    "example from chessgames.com" in {
      Reader(fromChessgames) must beSuccess
    }
    "immortal with NAG" in {
      Reader(withNag) must beSuccess
    }
    "example from TCEC" in {
      Reader(fromTcec) must beSuccess
    }
    "invalid variant" in {
      Reader(invalidVariant) must haveFailureMatching("Invalid variant")
    }
  }
}
