package chess
package format.pgn

import chess.variant.Standard
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import ChessTreeArbitraries.*

class ParserCheck extends ScalaCheckSuite:

  test("parse >>= render == identity"):
    forAll(genPgn(Situation(Standard))): pgn =>
      val str    = pgn.render
      val result = Parser.full(str).toOption.get.toPgn.render
      assertEquals(result, str)

  test("mainline == full.mainline"):
    forAll(genPgn(Situation(Standard))): pgn =>
      val str                                  = pgn.render
      val expected: Option[List[SanWithMetas]] = Parser.full(str).toOption.map(_.mainlineWithMetas)
      val mainline: Option[List[SanWithMetas]] = Parser.mainline(str).toOption.map(_.sans)
      assertEquals(mainline, expected)
