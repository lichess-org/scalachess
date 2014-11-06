package chess

import format.Visual.addNewLines
import Pos._

class GameTest extends ChessTest {

  "prevent castle by capturing a rook" should {
    val game = Game("""
 b
R   K""", Black)
    "can castle queenside" in {
      game.board.history canCastle White on QueenSide must_== true
    }
    "can still castle queenside" in {
      game.playMoves(B2 -> A3) must beSuccess.like {
        case g => g.board.history canCastle White on QueenSide must_== true
      }
    }
    "can not castle queenside anymore" in {
      game.playMoves(B2 -> A1) must beSuccess.like {
        case g => g.board.history canCastle White on QueenSide must_== false
      }
    }
  }
}
