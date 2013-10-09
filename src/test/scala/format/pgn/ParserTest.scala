package chess
package format.pgn

import Pos._

class ParserTest extends ChessTest {

  import Fixtures._

  val parser = Parser

  "promotion check" in {
    parser("b8=Q ") must beSuccess
  }

  "nags" in {
    parser(withNag) must beSuccess
  }

  raws foreach { sans ⇒
    val size = sans.split(' ').size
    "sans only size: " + size in {
      parser(sans) must beSuccess.like {
        case a ⇒ a.sans.size must_== size
      }
    }
  }

  "disambiguated" in {
    parser(disambiguated) must beSuccess.like {
      case a ⇒ a.sans.pp.size must_== 3
    }
  }

  List(fromProd1, fromProd2, castleCheck1, castleCheck2) foreach { sans ⇒
    val size = sans.split(' ').size
    "sans only from prod size: " + size in {
      parser(sans) must beSuccess.like {
        case a ⇒ a.sans.size must_== size
      }
    }
  }

  "variations" in {
    parser(variations) must beSuccess.like {
      case a => a.sans.size must_== 20
    }
  }

  "game from wikipedia" in {
    parser(fromWikipedia) must beSuccess.like {
      case a ⇒ a.sans.size must_== 85
    }
  }

  "inline comments" in {
    parser(inlineComments) must beSuccess.like {
      case a ⇒ a.sans.size must_== 85
    }
  }

  "comments and variations" in {
    parser(commentsAndVariations) must beSuccess.like {
      case a ⇒ a.sans.size must_== 103
    }
  }

  "comments and lines by smartchess" in {
    parser(bySmartChess) must beSuccess.like {
      case a ⇒ a.sans.size must_== 65
    }
  }

  "complete 960" in {
    parser(complete960) must beSuccess.like {
      case a ⇒ a.sans.size must_== 42
    }
  }
}
