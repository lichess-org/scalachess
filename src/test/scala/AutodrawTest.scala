package chess

import Pos._

class AutodrawTest extends ChessTest {

  "detect automatic draw" should {
    "by lack of pieces" in {
      "empty" in {
        makeEmptyBoard.autoDraw must_== true
      }
      "new" in {
        makeBoard.autoDraw must_== false
      }
      "opened" in {
        makeGame.playMoves(E2 -> E4, C7 -> C5, C2 -> C3, D7 -> D5, E4 -> D5) map { g =>
          g.board.autoDraw
        } must beSuccess(false)
      }
      "two kings" in {
      """
      k
K      """.autoDraw must_== true
      }
      "two kings and one pawn" in {
      """
  P   k
K      """.autoDraw must_== false
      }
      "two kings and one bishop" in {
      """
      k
K     B""".autoDraw must_== true
      }
      "two kings, one bishop and one knight of different colors" in {
      """
      k
K n   B""".autoDraw must_== false
      }
      "two kings, one bishop and one knight of same color" in {
      """
  B   k
K N    """.autoDraw must_== false
      }
      "two kings, one bishop and one rook of different colors" in {
      """
      k
K r   B""".autoDraw must_== false
      }
      "two kings, one bishop and one bishop of same colors" in {
      """
      k
K   b B""".autoDraw must_== true
      }
      "two kings, one bishop and one bishop of different colors" in {
      """
      k
K   bB""".autoDraw must_== false
      }
    }
    "by fifty moves" in {
      "new" in {
        makeBoard.autoDraw must_== false
      }
      "opened" in {
        makeGame.playMoves(E2 -> E4, C7 -> C5, C2 -> C3, D7 -> D5, E4 -> D5) map { g =>
          g.board.autoDraw
        } must beSuccess(false)
      }
      "tons of pointless moves" in {
        val moves = List.fill(30)(List(B1 -> C3, B8 -> C6, C3 -> B1, C6 -> B8))
        makeGame.playMoves(moves.flatten: _*) must beSuccess.like {
          case g => g.board.autoDraw must_== true
        }
      }
    }
  }
}
