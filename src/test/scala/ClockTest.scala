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
    def fakeTime(c: Clock) = c.copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    def advance(c: Clock, t: Int) = c.copy(timestamper = new Timestamper {
      val now = c.timestamper.now + Centis(t)
    })

    def durOf(lag: Int) = MoveMetrics(clientLag = Some(Centis(lag)))

    def clockStep(wait: Int, lags: Int*) = {
      (lags.foldLeft(fakeTime(Clock(60, 0)).start) { (clk, lag) =>
        (advance(clk.step().get, wait + lag) step durOf(lag)).get
      } remainingTime Black).centis
    }

    def clockStart(lag: Int) = {
      val clock = fakeTime(Clock(60, 0)).start.step().get
      ((clock step durOf(lag)).get remainingTime White).centis
    }

    "start" in {
      "no lag" in {
        clockStart(0) must_== 60 * 100
      }
      "small lag" in {
        clockStart(20) must_== 60 * 100
      }
      "big lag" in {
        clockStart(500) must_== 60 * 100
      }
    }

    "1 move" in {
      "premove, no lag" in {
        clockStep(0, 0) must_== 60 * 100
      }
      "premove, small lag" in {
        clockStep(0, 20) must_== 60 * 100
      }
      "premove, big lag" in {
        clockStep(0, 300) must_== 59 * 100
      }
      "1s move, no lag" in {
        clockStep(100, 0) must_== 59 * 100
      }
      "1s move, small lag" in {
        clockStep(100, 20) must_== 59 * 100
      }
      "1s move, big lag" in {
        clockStep(100, 300) must_== 58 * 100
      }
    }

    "multiple premoves" in {
      "no lag" in {
        clockStep(0, 0, 0) must_== 60 * 100
      }
      "medium lag x2" in {
        clockStep(0, 300, 300) must_== 57 * 100
      }
      "no -> medium lag" in {
        clockStep(0, 0, 300) must_== 60 * 100
      }
      "no x8 -> big lag" in {
        clockStep(0, 0, 0, 0, 0, 0, 0, 0, 0, 500) must_== 58 * 100
      }

      "no x5 -> big lag x2" in {
        clockStep(0, 0, 0, 0, 0, 0, 500, 500) must_== 56 * 100
      }

      "no x5 -> big lag x3" in {
        clockStep(0, 0, 0, 0, 0, 0, 500, 500, 500) must_== 52 * 100
      }
    }
  }
}
