package chess
package format.pgn

import chess.variant.Standard
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import ChessTreeArbitraries.*

class ParserCheck extends ScalaCheckSuite:

  test("parse >>= render == identity"):
    forAll(genPgn(Standard.initialPosition)): pgn =>
      val str = pgn.render
      val result = Parser.full(str).toOption.get.toPgn.render
      assertEquals(result, str)

  test("mainline == full.mainlineWithMetas"):
    forAll(genPgn(Standard.initialPosition)): pgn =>
      val str = pgn.render
      val expected = Parser.full(str).toOption.map(_.mainlineWithMetas)
      val mainline = Parser.mainlineWithMetas(str).toOption.map(_.moves)
      assertEquals(mainline, expected)

  test("mainlineWithSan == full.mainline"):
    forAll(genPgn(Standard.initialPosition)): pgn =>
      val str = pgn.render
      val expected = Parser.full(str).toOption.map(_.mainline)
      val mainline = Parser.mainline(str).toOption.map(_.moves)
      assertEquals(mainline, expected)
