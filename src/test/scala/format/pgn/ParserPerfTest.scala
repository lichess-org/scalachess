package chess
package format.pgn

import cats.implicits._

class ParserPerfTest extends ChessTest {

  val nb         = Fixtures.gamesForPerfTest.length
  val iterations = 10

  def run = Fixtures.gamesForPerfTest.traverse(Parser.full)

  "Parser perf" should {
    "many times" in {
      run must beValid
      if (nb * iterations > 1) {
        println("warming up")
        run
      }
      println("running tests")
      val durations = for (_ <- 1 to iterations) yield {
        val start = System.currentTimeMillis
        run
        val duration = System.currentTimeMillis - start
        println(s"$nb games in $duration ms")
        duration
      }
      val nbGames    = iterations * nb
      val moveMicros = (1000 * durations.sum) / nbGames
      println(s"Average = $moveMicros microseconds per game")
      println(s"          ${1000000 / moveMicros} games per second")
      true === true
    }
  }
}
