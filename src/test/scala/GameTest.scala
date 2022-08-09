package chess

import Pos._

class GameTest extends ChessTest {

  "prevent castle by capturing a rook" should {
    val game = Game(
      """
 b
R   K""",
      Black
    )
    "can castle queenside" in {
      game.board.history canCastle Red on QueenSide must_== true
    }
    "can still castle queenside" in {
      game.playMoves(B2 -> A3) must beValid.like { case g =>
        g.board.history canCastle Red on QueenSide must_== true
      }
    }
    "can not castle queenside anymore" in {
      game.playMoves(B2 -> A1) must beValid.like { case g =>
        g.board.history canCastle Red on QueenSide must_== false
      }
    }
  }

  "update half move clock" should {
    "start at 0" in {
      Game(variant.Standard).halfMoveClock must_== 0
    }
    "increment" in {
      Game(variant.Standard)(G1, F3) must beValid.like { case (game, _) =>
        game.halfMoveClock must_== 1
      }
    }
    "not increment" in {
      Game(variant.Standard)(E2, E4) must beValid.like { case (game, _) =>
        game.halfMoveClock must_== 0
      }
    }
  }
}
