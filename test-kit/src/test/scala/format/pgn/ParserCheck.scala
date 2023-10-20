package chess
package format.pgn

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import ChessTreeArbitraries.*
import PgnHelper.*
import chess.variant.Standard

class ParserCheck extends ScalaCheckSuite:

  test("ParserCheck"):
    forAll(genPgn(Situation(Standard))): pgn =>
      val str    = pgn.render
      val result = Parser.full(str).get.toPgn.render
      assertEquals(result, str)
