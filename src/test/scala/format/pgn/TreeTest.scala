package chess
package format.pgn

import munit.FunSuite
import chess.format.pgn.Fixtures

class TreeTest extends FunSuite:

  test("NewPgn and ParsedPgn are isomorphic"):
    Fixtures.gamesForPerfTest.foreach { str =>
      val parsedPgn = Parser.full(str).valueOr(err => throw new Exception(err.toString))
      val newPgn    = NewPgn.toNewPgn(parsedPgn)
      assertEquals(parsedPgn, newPgn.toParsedPgn)
    }

  // test("one game"):
  //   val str       = PgnStr(Fixtures.chessComCrazyhouse)
  //   val parsedPgn = Parser.full(str).valueOr(err => throw new Exception(err.toString))
  //   val newPgn    = NewPgn.toNewPgn(parsedPgn)
  //   assertEquals(parsedPgn.toString, newPgn.toParsedPgn.toString)
