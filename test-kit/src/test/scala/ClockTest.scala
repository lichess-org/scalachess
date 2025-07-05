package chess

import scala.language.implicitConversions

import Square.*

class ClockTest extends ChessTest:

  import clockConv.given

  val clock = Clock(5 * 60 * 1000, 0)
  val game  = makeGame.withClock(clock.start)

  test("play with a clock: new game"):
    assertEquals(game.clock.map { _.color }, Option(White))
  test("play with a clock: one move played"):
    assertEquals(game.playMoves(E2 -> E4).get.clock.map(_.color), Option(Black))
  test("create a clock: with time"):
    assertEquals(Clock(60, 10).limitSeconds, 60)
  test("create a clock: with increment"):
    assertEquals(Clock(60, 10).incrementSeconds, 10)
  test("create a clock: with few time"):
    assertEquals(Clock(0, 10).limitSeconds, 0)
  test("with 30 seconds"):
    assertEquals(Clock(30, 0).limitInMinutes, 0.5)

class ClockLagCompTest extends ChessTest:

  import clockConv.given

  val fakeClock60 = Clock(60, 0)
    .copy(timestamper =
      new Timestamper:
        val now = Timestamp(0)
    )
    .start

  val fakeClock600 = Clock(600, 0)
    .copy(timestamper =
      new Timestamper:
        val now = Timestamp(0)
    )
    .start

  def advance(c: Clock, t: Int) =
    c.copy(timestamper =
      new Timestamper:
        val now = c.timestamper.now + Centis(t)
    )

  def durOf(lag: Int) = MoveMetrics(clientLag = Option(Centis(lag)))

  def clockStep(clock: Clock, wait: Int, lags: Int*) =
    (lags
      .foldLeft(clock) { (clk, lag) =>
        advance(clk.step().value, wait + lag).step(durOf(lag)).value
      }
      .remainingTime(Black))
      .centis

  def clockStep60(w: Int, l: Int*)  = clockStep(fakeClock60, w, l*)
  def clockStep600(w: Int, l: Int*) = clockStep(fakeClock600, w, l*)

  def clockStart(lag: Int) =
    val clock = fakeClock60.step()
    ((clock.value.step(durOf(lag))).value.remainingTime(White)).centis

  test("start: no lag"):
    assertEquals(clockStart(0), 60 * 100)
  test("start: small lag"):
    assertEquals(clockStart(20), 60 * 100)
  test("start: big lag"):
    assertEquals(clockStart(500), 60 * 100)

  test("1 move: premove, no lag"):
    assertEquals(clockStep600(0, 0), 600 * 100)
  test("1 move: premove, small lag"):
    assertEquals(clockStep600(0, 20), 600 * 100)
  test("1 move: premove, big lag"):
    assertEquals(clockStep600(0, 400), 599 * 100)
  test("1 move: 1s move, no lag"):
    assertEquals(clockStep600(100, 0), 599 * 100)
  test("1 move: 1s move, small lag"):
    assertEquals(clockStep600(100, 20), 599 * 100)
  test("1 move: 1s move, big lag"):
    assertEquals(clockStep600(100, 400), 598 * 100)

  test("multiple premoves: no lag"):
    assertEquals(clockStep600(0, 0, 0), 600 * 100)
  test("multiple premoves: medium lag x2"):
    assertEquals(clockStep600(0, 300, 300), 598 * 100)
  test("multiple premoves: no -> medium lag"):
    assertEquals(clockStep600(0, 0, 300), 600 * 100)
  test("multiple premoves: no x8 -> big lag"):
    assertEquals(clockStep600(0, 0, 0, 0, 0, 0, 0, 0, 0, 800), 599 * 100)

  test("multiple premoves: no x5 -> big lag x2"):
    assertEquals(clockStep600(0, 0, 0, 0, 0, 0, 500, 600), 597 * 100)

  test("multiple premoves: no x5 -> big lag x3"):
    assertEquals(clockStep600(0, 0, 0, 0, 0, 0, 500, 500, 500), 594 * 100)

  test("multiple premoves with fast clock: no lag"):
    assertEquals(clockStep60(0, 0, 0), 60 * 100)
  test("multiple premoves with fast clock: no -> medium lag"):
    assertEquals(clockStep60(0, 0, 300), 5856)
  test("multiple premoves with fast clock: no x4 -> big lag"):
    assertEquals(clockStep60(0, 0, 0, 0, 0, 700), 5573)

  test("live time checks: 60s stall"):
    val clock60 = advance(fakeClock60, 60 * 100)

    assertEquals(clock60.remainingTime(White).centis, 0)
    assertNot(clock60.outOfTime(Black, withGrace = true))
    assertNot(clock60.outOfTime(White, withGrace = true))
    assert(clock60.outOfTime(White, withGrace = false))
  test("live time checks: 61s stall"):
    val clock61 = advance(fakeClock60, 61 * 100)
    assertEquals(clock61.remainingTime(White).centis, 0)
    assertNot(clock61.outOfTime(White, withGrace = true))
  test("live time checks: over quota stall"):
    assert(advance(fakeClock60, 6190).outOfTime(White, withGrace = true))
  test("live time checks: stall within quota"):
    assertNot(advance(fakeClock600, 60190).outOfTime(White, withGrace = true))
  test("live time checks: max grace stall"):
    assert(advance(fakeClock600, 602 * 100).outOfTime(White, withGrace = true))
