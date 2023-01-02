package chess
package bitboard

import munit.FunSuite
import org.lichess.compression.game.Board as CBoard

class FenTests extends FunSuite:

  import Helpers.*
  import Bitboard.*

  import scala.language.implicitConversions
  given Conversion[Pos, Int] = _.value

  test("checkers") {
    FenFixtures.fens.foreach { str =>
      val fen      = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val result   = fen.checkers.get
      val king     = fen.ourKing.get
      val expected = fen.cBoard.attacksTo(king, !fen.isWhiteTurn)
      assertEquals(result, expected.bb)
    }
  }
