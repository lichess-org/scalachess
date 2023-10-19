package chess

import scala.language.implicitConversions

class BerserkTest extends ChessTest:

  import clockConv.given

  def whiteBerserk(minutes: Int, seconds: Int) =
    Clock(minutes * 60, seconds).goBerserk(White).remainingTime(White).centis * .01

  test("berserkable: yep"):
    assertEquals(Clock.Config(60 * 60, 0).berserkable, true)
    assertEquals(Clock.Config(1 * 60, 0).berserkable, true)
    assertEquals(Clock.Config(60 * 60, 60).berserkable, true)
    assertEquals(Clock.Config(1 * 60, 0).berserkable, true)
  test("berserkable: nope"):
    assertEquals(Clock.Config(0 * 60, 1).berserkable, false)
    assertEquals(Clock.Config(0 * 60, 10).berserkable, false)
  test("berserk flags: white"):
    assertEquals(Clock(60, 0).berserked(White), false)
    assertEquals(Clock(60, 0).goBerserk(White).berserked(White), true)
  test("berserk flags: black"):
    assertEquals(Clock(60, 0).berserked(Black), false)
    assertEquals(Clock(60, 0).goBerserk(Black).berserked(Black), true)
  test("initial time penalty, no increment: 10+0"):
    assertEquals(whiteBerserk(10, 0), 5 * 60d)
  test("initial time penalty, no increment: 5+0"):
    assertEquals(whiteBerserk(5, 0), 2.5 * 60d)
  test("initial time penalty, no increment: 3+0"):
    assertEquals(whiteBerserk(3, 0), 1.5 * 60d)
  test("initial time penalty, no increment: 1+0"):
    assertEquals(whiteBerserk(1, 0), 0.5 * 60d)
  test("initial time penalty, with increment: 4+4"):
    assertEquals(whiteBerserk(4, 4), 2 * 60d)
  test("initial time penalty, with increment: 3+2"):
    assertEquals(whiteBerserk(3, 2), 1.5 * 60d)
  test("initial time penalty, with increment: 2+10"):
    assertEquals(whiteBerserk(2, 10), 2 * 60d)
  test("initial time penalty, with increment: 10+5"):
    assertEquals(whiteBerserk(10, 5), 5 * 60d)
  test("initial time penalty, with increment: 10+2"):
    assertEquals(whiteBerserk(10, 2), 5 * 60d)
  test("initial time penalty, with increment: 1+1"):
    assertEquals(whiteBerserk(1, 1), 0.5 * 60d)
  test("initial time penalty, with increment: 1+3"):
    assertEquals(whiteBerserk(1, 3), 1 * 60d)
  test("initial time penalty, with increment: 1+5"):
    assertEquals(whiteBerserk(1, 5), 1 * 60d)
