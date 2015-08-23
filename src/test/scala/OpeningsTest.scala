package chess

import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

class OpeningsTest extends Specification with Matchers {
  "codeFamily index" should {
    "correctly map a code to its respective family" in {
      Openings.codeFamily must havePair("E50" -> "Nimzo-Indian Defence")
    }
    "select the family with the highest cardinality for a given code" in {
      Openings.codeFamily must havePair("D02" -> "Queen Pawn Game")
    }
  }
}
