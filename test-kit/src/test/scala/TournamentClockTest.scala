package chess

import Clock.*

class TournamentClockTest extends ChessTest:

  def clock(minutes: Int, seconds: Int) = Some(
    TournamentClock(LimitSeconds(minutes * 60), IncrementSeconds(seconds))
  )

  test("parse"):
    import TournamentClock.parse

    assertEquals(parse("nope"), None)

    assertEquals(parse("5+5"), clock(5, 5))
    assertEquals(parse("5+0"), clock(5, 0))
    assertEquals(parse("30+40"), clock(30, 40))

    assertEquals(parse("15m + 10s"), clock(15, 10))
    assertEquals(parse("15 m + 10 s"), clock(15, 10))
    assertEquals(parse("15min + 10sec"), clock(15, 10))
    assertEquals(parse("15m + 10 sec"), clock(15, 10))
    assertEquals(parse("15 min + 10 sec"), clock(15, 10))
    assertEquals(parse("15 min + 10 s"), clock(15, 10))
    assertEquals(parse("15 minutes + 10 seconds"), clock(15, 10))

    assertEquals(parse("15 min + 10 sec / move"), clock(15, 10))
    assertEquals(parse("15 min + 10 s / move"), clock(15, 10))
    assertEquals(parse("15 min + 10 seconds / move"), clock(15, 10))
    assertEquals(parse("15 minutes + 10 seconds / move"), clock(15, 10))

    assertEquals(parse("90 min + 30 sec / move"), clock(90, 30))

    assertEquals(parse("120' + 12\""), clock(120, 12))
    assertEquals(parse("120' + 12\"/move"), clock(120, 12))
    assertEquals(parse("120' + 12\" / move"), clock(120, 12))
    assertEquals(parse("7200+12"), clock(120, 12))

    assertEquals(parse("3600"), clock(60, 0))

    // we're not there yet
    // assertEquals(parse("90 min / 40 moves + 30 min + 30 sec / move"), ???)
