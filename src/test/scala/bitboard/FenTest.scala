package chess
package bitboard

import munit.FunSuite
import org.lichess.compression.game.Board as CBoard

class FenTests extends FunSuite:

  import Helpers.*
  import StandardMovesGenerator.*
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

  /** board => legal moves => move => board \=> cmove => Cboard
    */
  test("play move") {
    val fen    = Fen.standard
    val cboard = fen.cBoard
    val result: Fen = List.range(0, 49).foldLeft(fen) { (fen, _) =>
      fen.generate match
        case Nil => fen
        case move :: _ =>
          cboard.play(move.cMove)
          val f = fen.play(move)
          assertFen(f, cboard)
          f
    }
    assertFen(result, cboard)
  }

  test("play move with fixtures") {
    FenFixtures.fens.foreach { str =>
      val fen    = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val cboard = fen.cBoard
      val result: Fen = List.range(0, 49).foldLeft(fen) { (fen, _) =>
        fen.generate match
          case Nil => fen
          case move :: _ =>
            cboard.play(move.cMove)
            val f = fen.play(move)
            assertFen(f, cboard)
            f
      }
      assertFen(result, cboard)
    }
  }

  private def assertFen(fen: Fen, cBoard: CBoard) =
    assertEquals(fen, cBoard.fen(fen.state.halfMoves, fen.state.fullMoves))
