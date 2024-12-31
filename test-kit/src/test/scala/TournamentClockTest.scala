package chess

import Clock.*

class TournamentClockTest extends ChessTest:

  import TournamentClock.parse

  def someClock(minutes: Int, seconds: Int) = Some:
    TournamentClock(LimitSeconds(minutes * 60), IncrementSeconds(seconds))

  test("parse"):
    assertEquals(parse(""), None)
    assertEquals(parse("nope"), None)

    assertEquals(parse("5+5"), someClock(5, 5))
    assertEquals(parse("5+0"), someClock(5, 0))
    assertEquals(parse("30+40"), someClock(30, 40))

    assertEquals(parse("15m + 10s"), someClock(15, 10))
    assertEquals(parse("15 m + 10 s"), someClock(15, 10))
    assertEquals(parse("15min + 10sec"), someClock(15, 10))
    assertEquals(parse("15m + 10 sec"), someClock(15, 10))
    assertEquals(parse("15 min + 10 sec"), someClock(15, 10))
    assertEquals(parse("15 min + 10 s"), someClock(15, 10))
    assertEquals(parse("15 minutes + 10 seconds"), someClock(15, 10))
    assertEquals(parse("  15    MiNUTes+10SECOnds  "), someClock(15, 10))

    assertEquals(parse("15 min + 10 sec / move"), someClock(15, 10))
    assertEquals(parse("15 min + 10 s / move"), someClock(15, 10))
    assertEquals(parse("15 min + 10 seconds / move"), someClock(15, 10))
    assertEquals(parse("15 minutes + 10 seconds / move"), someClock(15, 10))

    assertEquals(parse("90 min + 30 sec / move"), someClock(90, 30))

    assertEquals(parse("120' + 12\""), someClock(120, 12))
    assertEquals(parse("120' + 12\"/move"), someClock(120, 12))
    assertEquals(parse("120' + 12\" / move"), someClock(120, 12))
    assertEquals(parse("7200+12"), someClock(120, 12))

    assertEquals(parse("3600"), someClock(60, 0))
    assertEquals(parse("60"), someClock(60, 0))
    assertEquals(parse("180"), someClock(180, 0))
    assertEquals(parse("240"), someClock(4, 0))

    assertEquals(parse("120 min / 40 moves + 30 min"), None)

    // we're not there yet
    // assertEquals(parse("90 min / 40 moves + 30 min + 30 sec / move"), ???)
