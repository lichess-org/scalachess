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
      "one pawn" in {
        """
  P   k
K      """.autoDraw must_== false
      }
      "one bishop" in {
        """
      k
K     B""".autoDraw must_== true
      }
      "one bishop and one knight of different colors" in {
        """
      k
K n   B""".autoDraw must_== false
      }
      "one bishop and one knight of same color" in {
        """
  B   k
K N    """.autoDraw must_== false
      }
      "one bishop and one rook of different colors" in {
        """
      k
K r   B""".autoDraw must_== false
      }
      "one bishop and one bishop of same colors" in {
        """
      k
K   b B""".autoDraw must_== true
      }
      "one bishop and one bishop of different colors" in {
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
    "by threefold" in {
      "from prod should 3fold" in {
        val moves = List(E2 -> E4, E7 -> E6, F2 -> F4, C7 -> C5, E4 -> E5, D7 -> D6, G1 -> F3, B8 -> C6, F1 -> B5, C8 -> D7, B5 -> C6, D7 -> C6, D2 -> D3, C6 -> F3, D1 -> F3, D6 -> D5, E1 -> H1, D8 -> B6, C1 -> E3, B6 -> B2, B1 -> D2, B2 -> B6, A1 -> B1, B6 -> C7, C2 -> C4, A8 -> D8, C4 -> D5, D8 -> D5, D2 -> E4, B7 -> B6, F1 -> D1, G8 -> H6, G2 -> G4, F8 -> E7, G1 -> G2, E8 -> H8, H2 -> H3, F8 -> D8, B1 -> B3, C5 -> C4, B3 -> C3, E7 -> B4, C3 -> C1, D5 -> D3, D1 -> D3, D8 -> D3, C1 -> C2, C4 -> C3, H3 -> H4, C7 -> C6, E4 -> F6, G7 -> F6, F3 -> C6, D3 -> E3, E5 -> F6, H6 -> G4, C6 -> C8, B4 -> F8, C2 -> C3, E3 -> E4, G2 -> F3, G4 -> F6, C8 -> D8, G8 -> G7, D8 -> F6, G7 -> F6, F3 -> E4, F8 -> C5, A2 -> A4, F6 -> G6, A4 -> A5, G6 -> H5, A5 -> B6, A7 -> B6, C3 -> G3, H5 -> H4, G3 -> G7, H7 -> H5, G7 -> F7, H4 -> G3, F7 -> F6, H5 -> H4, F6 -> E6, H4 -> H3, E6 -> G6, G3 -> F2, G6 -> H6, F2 -> G2, H6 -> G6, G2 -> F2, G6 -> H6, F2 -> G2, H6 -> G6, G2 -> F2, G6 -> H6, F2 -> G2, H6 -> G6)
        makeGame.playMoves(moves: _*) must beSuccess.like {
          case g => g.board.history.threefoldRepetition must beTrue
        }
      }
      "from prod should not 3fold" in {
        val moves = List(E2 -> E4, E7 -> E6, F2 -> F4, C7 -> C5, E4 -> E5, D7 -> D6, G1 -> F3, B8 -> C6, F1 -> B5, C8 -> D7, B5 -> C6, D7 -> C6, D2 -> D3, C6 -> F3, D1 -> F3, D6 -> D5, E1 -> H1, D8 -> B6, C1 -> E3, B6 -> B2, B1 -> D2, B2 -> B6, A1 -> B1, B6 -> C7, C2 -> C4, A8 -> D8, C4 -> D5, D8 -> D5, D2 -> E4, B7 -> B6, F1 -> D1, G8 -> H6, G2 -> G4, F8 -> E7, G1 -> G2, E8 -> H8, H2 -> H3, F8 -> D8, B1 -> B3, C5 -> C4, B3 -> C3, E7 -> B4, C3 -> C1, D5 -> D3, D1 -> D3, D8 -> D3, C1 -> C2, C4 -> C3, H3 -> H4, C7 -> C6, E4 -> F6, G7 -> F6, F3 -> C6, D3 -> E3, E5 -> F6, H6 -> G4, C6 -> C8, B4 -> F8, C2 -> C3, E3 -> E4, G2 -> F3, G4 -> F6, C8 -> D8, G8 -> G7, D8 -> F6, G7 -> F6, F3 -> E4, F8 -> C5, A2 -> A4, F6 -> G6, A4 -> A5, G6 -> H5, A5 -> B6, A7 -> B6, C3 -> G3, H5 -> H4, G3 -> G7, H7 -> H5, G7 -> F7, H4 -> G3, F7 -> F6, H5 -> H4, F6 -> E6, H4 -> H3, E6 -> G6, G3 -> F2, G6 -> H6, F2 -> G2, H6 -> G6, G2 -> F2, G6 -> H6)
        makeGame.playMoves(moves: _*) must beSuccess.like {
          case g => g.board.history.threefoldRepetition must beFalse
        }
      }
      // "3fold on initial position - broken" in {
      //   val moves = List.fill(2)(List(G1 -> F3, B8 -> C6, F3 -> G1, C6 -> B8)).flatten
      //   makeGame.playMoves(moves: _*) must beSuccess.like {
      //     case g => g.board.history.threefoldRepetition must beTrue
      //   }
      // }
      "pawn move then minimalist 3fold" in {
        val moves = List(E2 -> E4, E7 -> E5) ::: List.fill(2)(List(G1 -> F3, B8 -> C6, F3 -> G1, C6 -> B8)).flatten
        makeGame.playMoves(moves: _*) must beSuccess.like {
          case g => g.board.history.threefoldRepetition must beTrue
        }
      }
    }
  }
}
