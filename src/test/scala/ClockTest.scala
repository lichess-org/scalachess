package chess

import Pos._

class ClockTest extends ChessTest {

  "play with a clock" should {
    val clock = Clock(5 * 60 * 1000, 0)
    val game = Game() withClock clock
    "new game" in {
      game.clock must beSome.like {
        case c ⇒ c.color must_== White
      }
    }
    "one move played" in {
      game.playMoves(E2 -> E4) must beSuccess.like {
        case g ⇒ g.clock must beSome.like {
          case c ⇒ c.color must_== Black
        }
      }
    }
  }
  "create a clock" should {
    "with time" in {
      Clock(60, 10).limit must_== 60
    }
    "with increment" in {
      Clock(60, 10).increment must_== 10
    }
    "with few time" in {
      Clock(0, 10).limit must_== 2
    }
  }
  "lag compensation" should {
    def clockStep(wait: Float, lag: Float): Double = {
      val clock = Clock(60, 0) step 0
      Thread sleep (wait * 1000).toInt
      (clock step lag remainingTime Black).toDouble
    }
    def clockStart(lag: Float): Double = {
      val clock = Clock(60, 0) step lag
      (clock step lag remainingTime White).toDouble
    }
    val delta = 0.05
    val maxLag = Clock.maxLagToCompensate
    "premove, no lag" in {
      clockStep(0, 0) must beCloseTo(60, delta)
    }
    "premove, small lag" in {
      clockStep(0, 0.2f) must beCloseTo(60, delta)
    }
    "premove, big lag" in {
      clockStep(0, maxLag * 5) must beCloseTo(60, delta)
    }
    "1s move, no lag" in {
      clockStep(1f, 0) must beCloseTo(59, delta)
    }
    "1s move, small lag" in {
      clockStep(1f, 0.2f) must beCloseTo(59.2, delta)
    }
    "1s move, big lag" in {
      clockStep(1f, maxLag * 5) must beCloseTo(59 + maxLag, delta)
    }
    "start, no lag" in {
      clockStart(0) must beCloseTo(60, delta)
    }
    "start, small lag" in {
      clockStart(0.2f) must beCloseTo(60, delta)
    }
    "start, big lag" in {
      clockStart(maxLag * 5) must beCloseTo(60, delta)
    }
  }
}
