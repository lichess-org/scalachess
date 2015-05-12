package chess
package format.pgn

import Pos._

class ReaderTest extends ChessTest {

  import Fixtures._

  "only raw moves" should {
    "many games" in {
      forall(raws) { (c: String) =>
        Reader.full(c) must beSuccess.like {
          case replay => replay.moves must have size (c.split(' ').size)
        }
      }
    }
    "example from prod 1" in {
      Reader.full(fromProd1) must beSuccess
    }
    "example from prod 2" in {
      Reader.full(fromProd2) must beSuccess
    }
    "rook promotion" in {
      Reader.full(promoteRook) must beSuccess
    }
    "castle check O-O-O+" in {
      Reader.full(castleCheck1) must beSuccess
    }
    "castle checkmate O-O#" in {
      Reader.full(castleCheck2) must beSuccess
    }
    "and delimiters" in {
      Reader.full(withDelimiters) must beSuccess.like {
        case replay => replay.moves must have size 33
      }
    }
    "and delimiters on new lines" in {
      Reader.full(withDelimitersOnNewLines) must beSuccess.like {
        case replay => replay.moves must have size 33
      }
    }
  }
  "tags and moves" should {
    "chess960" in {
      Reader.full(complete960) must beSuccess
    }
    "with empty lines" in {
      Reader.full("\n" + complete960 + "\n") must beSuccess
    }
    "example from wikipedia" in {
      Reader.full(fromWikipedia) must beSuccess
    }
    "with inline comments" in {
      Reader.full(inlineComments) must beSuccess
    }
    "example from chessgames.com" in {
      Reader.full(fromChessgames) must beSuccess
    }
    "example from chessgames.com with escape chars" in {
      Reader.full(fromChessgamesWithEscapeChar) must beSuccess
    }
    "immortal with NAG" in {
      Reader.full(withNag) must beSuccess
    }
    "example from TCEC" in {
      Reader.full(fromTcec) must beSuccess
    }
    "comments and variations" in {
      Reader.full(commentsAndVariations) must beSuccess
    }
    "comments and variations by smartchess" in {
      Reader.full(bySmartChess) must beSuccess
    }
    "invalid variant" in {
      Reader.full(invalidVariant) must beSuccess.like {
        case replay => replay.setup.board.variant must_== variant.Standard
      }
    }
    "promoting to a rook" in {
      Reader.full(fromLichessBadPromotion) must beSuccess.like {
        case replay => replay.chronoMoves lift 10 must beSome.like {
          case move => move.promotion must_== Some(Rook)
        }
      }
    }
    "atomic regression" in {
      Reader.full(atomicRegression) must beSuccess
    }
    "atomic promotion" in {
      Reader.full(atomicPromotion) must beSuccess
    }
  }
}
