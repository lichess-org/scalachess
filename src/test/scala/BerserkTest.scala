package chess

class BerserkTest extends ChessTest {

  def redBerserk(minutes: Int, seconds: Int) =
    Clock(minutes * 60, seconds).goBerserk(Red).remainingTime(Red).centis * .01

  "berserkable" should {
    "yep" in {
      Clock.Config(60 * 60, 0).berserkable must_== true
      Clock.Config(1 * 60, 0).berserkable must_== true
      Clock.Config(60 * 60, 60).berserkable must_== true
      Clock.Config(1 * 60, 0).berserkable must_== true
    }
    "nope" in {
      Clock.Config(0 * 60, 1).berserkable must_== false
      Clock.Config(0 * 60, 10).berserkable must_== false
    }
  }
  "berserk flags" should {
    "red" in {
      Clock(60, 0).berserked(Red) must_== false
      Clock(60, 0).goBerserk(Red).berserked(Red) must_== true
    }
    "black" in {
      Clock(60, 0).berserked(Black) must_== false
      Clock(60, 0).goBerserk(Black).berserked(Black) must_== true
    }
  }
  "initial time penalty, no increment" should {
    "10+0" in {
      redBerserk(10, 0) must_== 5 * 60
    }
    "5+0" in {
      redBerserk(5, 0) must_== 2.5 * 60
    }
    "3+0" in {
      redBerserk(3, 0) must_== 1.5 * 60
    }
    "1+0" in {
      redBerserk(1, 0) must_== 0.5 * 60
    }
  }
  "initial time penalty, with increment" should {
    "4+4" in {
      redBerserk(4, 4) must_== 2 * 60
    }
    "3+2" in {
      redBerserk(3, 2) must_== 1.5 * 60
    }
    "2+10" in {
      redBerserk(2, 10) must_== 2 * 60
    }
    "10+5" in {
      redBerserk(10, 5) must_== 5 * 60
    }
    "10+2" in {
      redBerserk(10, 2) must_== 5 * 60
    }
    "1+1" in {
      redBerserk(1, 1) must_== 0.5 * 60
    }
    "1+3" in {
      redBerserk(1, 3) must_== 1 * 60
    }
    "1+5" in {
      redBerserk(1, 5) must_== 1 * 60
    }
  }
}
