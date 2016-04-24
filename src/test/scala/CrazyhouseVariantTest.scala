package chess

import scala.collection.immutable.HashSet
import variant.Crazyhouse

class CrazyhouseVariantTest extends ChessTest {

  "Crazyhouse" should {

    "nothing to drop" in {
      val fenPosition = "3Nkb1r/1pQP1ppp/4p3/3N4/N5N1/6B1/PPPPBPPP/R1B2RK1 b - - 0 25"
      val game = {
        fenToGame(fenPosition, Crazyhouse).toOption err "ooopses"
      }.updateBoard { b =>
        b.withCrazyData(Crazyhouse.Data(
          pockets = Crazyhouse.Pockets(
            Crazyhouse.Pocket(Nil),
            Crazyhouse.Pocket(Nil)),
          promoted = Set.empty))
      }
      game.situation.checkMate must beTrue
    }

    "pieces to drop, in vain" in {
      val fenPosition = "3Nkb1r/1pQP1ppp/4p3/3N4/N5N1/6B1/PPPPBPPP/R1B2RK1 b - - 0 25"
      val game = {
        fenToGame(fenPosition, Crazyhouse).toOption err "ooopses"
      }.updateBoard { b =>
        b.withCrazyData(Crazyhouse.Data(
          pockets = Crazyhouse.Pockets(
            Crazyhouse.Pocket(Queen :: Nil),
            Crazyhouse.Pocket(Rook :: Pawn :: Pawn :: Nil)),
          promoted = Set.empty))
      }
      game.situation.checkMate must beTrue
    }

    "autodraw" in {
      import Pos._
      "tons of pointless moves but shouldn't apply 50-moves" in {
        val moves = List.fill(30)(List(B1 -> C3, B8 -> C6, C3 -> B1, C6 -> B8))
        Game(Crazyhouse).playMoves(moves.flatten: _*) must beSuccess.like {
          case g => g.board.autoDraw must beFalse
        }
      }
      "from prod should 3fold" in {
        val moves = List(E2 -> E4, E7 -> E6, F2 -> F4, C7 -> C5, E4 -> E5, D7 -> D6, G1 -> F3, B8 -> C6, F1 -> B5, C8 -> D7, B5 -> C6, D7 -> C6, D2 -> D3, C6 -> F3, D1 -> F3, D6 -> D5, E1 -> H1, D8 -> B6, C1 -> E3, B6 -> B2, B1 -> D2, B2 -> B6, A1 -> B1, B6 -> C7, C2 -> C4, A8 -> D8, C4 -> D5, D8 -> D5, D2 -> E4, B7 -> B6, F1 -> D1, G8 -> H6, G2 -> G4, F8 -> E7, G1 -> G2, E8 -> H8, H2 -> H3, F8 -> D8, B1 -> B3, C5 -> C4, B3 -> C3, E7 -> B4, C3 -> C1, D5 -> D3, D1 -> D3, D8 -> D3, C1 -> C2, C4 -> C3, H3 -> H4, C7 -> C6, E4 -> F6, G7 -> F6, F3 -> C6, D3 -> E3, E5 -> F6, H6 -> G4, C6 -> C8, B4 -> F8, C2 -> C3, E3 -> E4, G2 -> F3, G4 -> F6, C8 -> D8, G8 -> G7, D8 -> F6, G7 -> F6, F3 -> E4, F8 -> C5, A2 -> A4, F6 -> G6, A4 -> A5, G6 -> H5, A5 -> B6, A7 -> B6, C3 -> G3, H5 -> H4, G3 -> G7, H7 -> H5, G7 -> F7, H4 -> G3, F7 -> F6, H5 -> H4, F6 -> E6, H4 -> H3, E6 -> G6, G3 -> F2, G6 -> H6, F2 -> G2, H6 -> G6, G2 -> F2, G6 -> H6, F2 -> G2, H6 -> G6, G2 -> F2, G6 -> H6, F2 -> G2, H6 -> G6)
        Game(Crazyhouse).playMoves(moves: _*) must beSuccess.like {
          case g => g.situation.threefoldRepetition must beTrue
        }
      }
      "from prod should not 3fold" in {
        val moves = List(E2 -> E4, E7 -> E6, F2 -> F4, C7 -> C5, E4 -> E5, D7 -> D6, G1 -> F3, B8 -> C6, F1 -> B5, C8 -> D7, B5 -> C6, D7 -> C6, D2 -> D3, C6 -> F3, D1 -> F3, D6 -> D5, E1 -> H1, D8 -> B6, C1 -> E3, B6 -> B2, B1 -> D2, B2 -> B6, A1 -> B1, B6 -> C7, C2 -> C4, A8 -> D8, C4 -> D5, D8 -> D5, D2 -> E4, B7 -> B6, F1 -> D1, G8 -> H6, G2 -> G4, F8 -> E7, G1 -> G2, E8 -> H8, H2 -> H3, F8 -> D8, B1 -> B3, C5 -> C4, B3 -> C3, E7 -> B4, C3 -> C1, D5 -> D3, D1 -> D3, D8 -> D3, C1 -> C2, C4 -> C3, H3 -> H4, C7 -> C6, E4 -> F6, G7 -> F6, F3 -> C6, D3 -> E3, E5 -> F6, H6 -> G4, C6 -> C8, B4 -> F8, C2 -> C3, E3 -> E4, G2 -> F3, G4 -> F6, C8 -> D8, G8 -> G7, D8 -> F6, G7 -> F6, F3 -> E4, F8 -> C5, A2 -> A4, F6 -> G6, A4 -> A5, G6 -> H5, A5 -> B6, A7 -> B6, C3 -> G3, H5 -> H4, G3 -> G7, H7 -> H5, G7 -> F7, H4 -> G3, F7 -> F6, H5 -> H4, F6 -> E6, H4 -> H3, E6 -> G6, G3 -> F2, G6 -> H6, F2 -> G2, H6 -> G6, G2 -> F2, G6 -> H6)
        Game(Crazyhouse).playMoves(moves: _*) must beSuccess.like {
          case g => g.situation.threefoldRepetition must beFalse
        }
      }
    }
  }
}
