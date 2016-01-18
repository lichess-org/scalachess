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
  }
}
