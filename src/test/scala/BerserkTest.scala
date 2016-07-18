package chess

import Pos._

class BerserkTest extends ChessTest {

    def whiteBerserk(minutes: Int, seconds: Int) =
      Clock(minutes * 60, seconds).berserk(White).remainingTime(White)

  "berserkable" should {
    "yep" in {
      Clock(60 * 60, 0).berserkable must_== true
      Clock(1 * 60, 0).berserkable must_== true
      Clock(60 * 60, 60).berserkable must_== true
      Clock(1 * 60, 0).berserkable must_== true
    }
    "nope" in {
      Clock(0 * 60, 1).berserkable must_== false
      Clock(0 * 60, 10).berserkable must_== false
    }
  }
  "berserk flags" should {
    "white" in {
      Clock(60, 0).whiteBerserk must_== false
      Clock(60, 0).berserk(White).whiteBerserk must_== true
    }
    "black" in {
      Clock(60, 0).blackBerserk must_== false
      Clock(60, 0).berserk(Black).blackBerserk must_== true
    }
  }
  "initial time penalty, no increment" should {
    "10+0" in {
      whiteBerserk(10, 0) must_== 5 * 60
    }
    "5+0" in {
      whiteBerserk(5, 0) must_== 2.5 * 60
    }
    "3+0" in {
      whiteBerserk(3, 0) must_== 1.5 * 60
    }
    "1+0" in {
      whiteBerserk(1, 0) must_== 0.5 * 60
    }
  }
  "initial time penalty, with increment" should {
    "4+4" in {
      whiteBerserk(4, 4) must_== 2 * 60
    }
    "3+2" in {
      whiteBerserk(3, 2) must_== 1.5 * 60
    }
    "2+10" in {
      whiteBerserk(2, 10) must_== 2 * 60
    }
    "10+5" in {
      whiteBerserk(10, 5) must_== 5 * 60
    }
    "10+2" in {
      whiteBerserk(10, 2) must_== 5 * 60
    }
    "1+1" in {
      whiteBerserk(1, 1) must_== 0.5 * 60
    }
    "1+3" in {
      whiteBerserk(1, 3) must_== 1 * 60
    }
    "1+5" in {
      whiteBerserk(1, 5) must_== 1 * 60
    }
  }
}
