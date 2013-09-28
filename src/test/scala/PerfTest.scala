package chess

import Pos._

class PerfTest extends ChessTest {

  val nb = 10
  val iterations = 10
  // val moves = Seq(E2 -> E4, D7 -> D5, E4 -> D5, D8 -> D5, B1 -> C3, D5 -> A5, D2 -> D4, C7 -> C6, G1 -> F3, C8 -> G4, C1 -> F4, E7 -> E6, H2
  //   -> H3, G4 -> F3, D1 -> F3, F8 -> B4, F1 -> E2, B8 -> D7, A2 -> A3, E8 -> C8, A3 -> B4, A5 -> A1, E1 -> D2, A1 -> H1, F3 -> C6, B7 -> C6,
  //   E2 -> A6)
  // val moves = Seq(E2 -> E4, C7 -> C5, C2 -> C3, D7 -> D5, E4 -> D5, D8 -> D5, D2 -> D4, G8 -> F6, G1 -> F3, C8 -> G4, F1 -> E2, E7 -> E6, H2 -> H3, G4 -> H5, E1 -> G1, B8 -> C6, C1 -> E3, C5 -> D4, C3 -> D4, F8 -> B4) 
  val moves = format.pgn.Reader(
    format.pgn.Fixtures.fromChessgames
  ).toOption.get.chronoMoves map { m => m.orig -> m.dest }
  val game = makeGame

  def runOne = game.playMoves(moves: _*)
  def run { for (i ← 1 to nb) runOne }

  "playing a game" should {
    "many times" in {
      runOne must beSuccess
      println("warming up")
      run
      println("running tests")
      val durations = for (i ← 1 to iterations) yield {
        val start = System.currentTimeMillis
        run
        val duration = System.currentTimeMillis - start
        println(s"$nb games in $duration ms")
        duration
      }
      println(s"Average = ${(1000 * durations.sum) / iterations / nb / moves.size} microseconds per move")
      println(s"Global count = $globalCount")
      true === true
    }
  }
}
