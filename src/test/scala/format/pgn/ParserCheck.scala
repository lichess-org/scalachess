package chess
package format.pgn

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import TreeArbitraries.given
import PgnHelper.*

class ParserCheck extends ScalaCheckSuite:

  test("ParserCheck"):
    forAll: (pgn: Pgn) =>
      val str    = pgn.render
      val result = Parser.full(str).toOption.get.toPgn.render
      assertEquals(result, str)
