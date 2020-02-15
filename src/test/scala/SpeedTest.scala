import chess.Speed._
import chess.{ ChessTest, Clock, Speed }

class SpeedTest extends ChessTest {

  "Finding a Speed" should {
    "return each Speed by ID" in {
      Speed(0).get must_== UltraBullet
      Speed(1).get must_== Bullet
      Speed(2).get must_== Blitz
      Speed(3).get must_== Classical
      Speed(4).get must_== Correspondence
      Speed(5).get must_== Rapid
    }

    "return None if the Speed cannot be found" in {
      val invalidId = 42
      Speed(invalidId) must beNone
    }

    "return true if the ID exists" in {
      val validId = 2
      Speed.exists(validId) must_== true
    }

    "return false if the ID does not exist" in {
      val invalidId = 42
      Speed.exists(invalidId) must_== false
    }

    "find a Speed by using a Clock Config" in {
      Speed(clockWithTimeLimit(15)) must_== UltraBullet
      Speed(clockWithTimeLimit(90)) must_== Bullet
      Speed(clockWithTimeLimit(240)) must_== Blitz
      Speed(clockWithTimeLimit(750)) must_== Rapid
      Speed(clockWithTimeLimit(10800)) must_== Classical
      Speed(clockWithTimeLimit(100000)) must_== Correspondence
    }

    "find a Speed by using an Option of a Clock Config" in {
      Speed(Some(clockWithTimeLimit(0))) must_== UltraBullet
      Speed(Some(clockWithTimeLimit(30))) must_== Bullet
      Speed(Some(clockWithTimeLimit(180))) must_== Blitz
      Speed(Some(clockWithTimeLimit(480))) must_== Rapid
      Speed(Some(clockWithTimeLimit(1500))) must_== Classical
      Speed(Some(clockWithTimeLimit(21600))) must_== Correspondence
    }

    "return Correspondence if the Clock Config is None" in {
      Speed(None) must_== Correspondence
    }

    "find a Speed by time in seconds" in {
      Speed.byTime(29) must_== UltraBullet
      Speed.byTime(179) must_== Bullet
      Speed.byTime(479) must_== Blitz
      Speed.byTime(1499) must_== Rapid
      Speed.byTime(21599) must_== Classical
      Speed.byTime(Int.MaxValue) must_== Correspondence
    }

    "return Correspondence if the time is out of range" in {
      Speed.byTime(Int.MinValue) must_== Correspondence
    }
  }

  "Comparing two Speeds" should {
    "return -1 if the first is faster than the second" in {
      Blitz compare Rapid must_== -1
    }

    "return 0 if they equal" in {
      Blitz compare Blitz must_== 0
    }

    "return 1 if the first is slower than the second" in {
      Blitz compare Bullet must_== 1
    }
  }

  private def clockWithTimeLimit(limitSeconds: Int) =
    Clock.Config(limitSeconds = limitSeconds, incrementSeconds = 0)
}
