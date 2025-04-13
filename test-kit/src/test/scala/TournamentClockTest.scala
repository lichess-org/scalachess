package chess

import Clock.*

class TournamentClockTest extends ChessTest:

  val parse       = TournamentClock.parse(false)
  val parseStrict = TournamentClock.parse(true)

  def someClock(seconds: Int, inc: Int) = Some:
    TournamentClock(LimitSeconds(seconds), IncrementSeconds(inc))

  test("parse empty"):
    assertEquals(parse(""), None)
    assertEquals(parse("nope"), None)

  test("parse standard"):
    assertEquals(parse("300+5"), someClock(300, 5))
    assertEquals(parse("300+0"), someClock(300, 0))
    assertEquals(parse("5400+60"), someClock(5400, 60))

  test("parse as minutes for compat"):
    assertEquals(parse("3+2"), someClock(3 * 60, 2))
    assertEquals(parse("60+30"), someClock(60 * 60, 30))
    assertEquals(parse("180+30"), someClock(180 * 60, 30))

  test("parse strict"):
    assertEquals(parseStrict("60+0"), someClock(60, 0))
    assertEquals(parseStrict("120+1"), someClock(120, 1))

  test("parse weird shit"):
    assertEquals(parse("15m + 10s"), someClock(15 * 60, 10))
    assertEquals(parse("15 m + 10 s"), someClock(15 * 60, 10))
    assertEquals(parse("15min + 10sec"), someClock(15 * 60, 10))
    assertEquals(parse("15m + 10 sec"), someClock(15 * 60, 10))
    assertEquals(parse("15 min + 10 sec"), someClock(15 * 60, 10))
    assertEquals(parse("15 min + 10 s"), someClock(15 * 60, 10))
    assertEquals(parse("15 minutes + 10 seconds"), someClock(15 * 60, 10))
    assertEquals(parse("  15    MiNUTes+10SECOnds  "), someClock(15 * 60, 10))

    assertEquals(parse("15 min + 10 sec / move"), someClock(15 * 60, 10))
    assertEquals(parse("15 min + 10 s / move"), someClock(15 * 60, 10))
    assertEquals(parse("15 min + 10 seconds / move"), someClock(15 * 60, 10))
    assertEquals(parse("15 minutes + 10 seconds / move"), someClock(15 * 60, 10))

    assertEquals(parse("90 min + 30 sec / move"), someClock(90 * 60, 30))

    assertEquals(parse("120' + 12\""), someClock(120 * 60, 12))
    assertEquals(parse("120' + 12\"/move"), someClock(120 * 60, 12))
    assertEquals(parse("120' + 12\" / move"), someClock(120 * 60, 12))

    assertEquals(parse("3600"), someClock(3600, 0))
    assertEquals(parse("60"), someClock(60 * 60, 0))
    assertEquals(parse("180"), someClock(180 * 60, 0))
    assertEquals(parse("240"), someClock(240, 0))

    assertEquals(parse("120 min / 40 moves + 30 min"), None)

    // we're not there yet
    // assertEquals(parse("90 min / 40 moves + 30 min + 30 sec / move"), ???)
