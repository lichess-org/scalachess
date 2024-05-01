package chess
package format.pgn

import chess.variant.Standard
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import ChessTreeArbitraries.*

class ParserCheck extends ScalaCheckSuite:

  test("ParserCheck"):
    forAll(genPgn(Situation(Standard))): pgn =>
      val str    = pgn.render
      val result = Parser.full(str).toOption.get.toPgn.render
      assertEquals(result, str)
