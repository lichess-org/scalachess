package chess

class BoardReplayPerfTest extends ChessTest {

  args(skipAll = true)

  val nb = 100
  val iterations = 10
  // val nb = 1
  // val iterations = 1

  val moves = format.pgn.Fixtures.fromProd2.split(' ').toList
  def runOne = Replay.boards(moves, None, variant.Standard)
  def run: Unit = { for (i <- 1 to nb) runOne }

  "playing a game" should {
    "many times" in {
      runOne must beSuccess
      if (nb * iterations > 1) {
        println("warming up")
        run
      }
      println("running tests")
      val durations = for (i <- 1 to iterations) yield {
        val start = System.currentTimeMillis
        run
        val duration = System.currentTimeMillis - start
        println(s"$nb games in $duration ms")
        duration
      }
      val nbGames = iterations * nb
      val moveMicros = (1000 * durations.sum) / nbGames
      println(s"Average = $moveMicros microseconds per game")
      println(s"          ${1000000 / moveMicros} games per second")
      true === true
    }
  }
}
