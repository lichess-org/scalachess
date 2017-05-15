package chess

import Pos._

class ClockTest extends ChessTest {

  "play with a clock" should {
    val clock = Clock(5 * 60 * 1000, 0)
    val game = makeGame withClock clock.start
    "new game" in {
      game.clock map { _.color } must_== Some(White)
    }
    "one move played" in {
      game.playMoves(E2 -> E4) must beSuccess.like {
        case g: Game => g.clock map { _.color } must_== Some(Black)
      }
    }
  }
  "create a clock" should {
    "with time" in {
      Clock(60, 10).limitSeconds must_== 60
    }
    "with increment" in {
      Clock(60, 10).incrementSeconds must_== 10
    }
    "with few time" in {
      Clock(0, 10).limitSeconds must_== 0
    }
    "with 30 seconds" in {
      Clock(30, 0).limitInMinutes must_== 0.5
    }
  }
  "lag compensation" should {
    def fakeTime(c: RunningClock, t: Int) = c.copy(timestamper = new Timestamper {
      def now = Timestamp(10 * t)
    })

    def fakeTimeP(c: PausedClock) = c.copy(timestamper = new Timestamper {
      def now = Timestamp(0)
    })

    def durOf(lag: Int) = Centis(lag)
    def clockStep(wait: Int, lag: Int) = {
      val clock = fakeTimeP(Clock(60, 0)).start.step()
      val clockStep = fakeTime(clock, wait + lag) step durOf(lag)
      (clockStep remainingTime Black).centis
    }
    def clockStart(lag: Int) = {
      val clock = fakeTimeP(Clock(60, 0)).start.step()
      (clock step durOf(lag) remainingTime White).centis
    }

    "premove, no lag" in {
      clockStep(0, 0) must_== 60 * 100
    }
    "premove, small lag" in {
      clockStep(0, 20) must_== 60 * 100
    }
    "premove, big lag" in {
      clockStep(0, 200) must_== 59 * 100
    }
    "1s move, no lag" in {
      clockStep(100, 0) must_== 59 * 100
    }
    "1s move, small lag" in {
      clockStep(100, 20) must_== 59 * 100
    }
    "1s move, big lag" in {
      clockStep(100, 200) must_== 58 * 100
    }
    "start, no lag" in {
      clockStart(0) must_== 60 * 100
    }
    "start, small lag" in {
      clockStart(20) must_== 60 * 100
    }
    "start, big lag" in {
      clockStart(500) must_== 60 * 100
    }
  }
}
