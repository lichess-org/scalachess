package chess

import chess.Status._
import chess.{ ChessTest, Status }

class StatusTest extends ChessTest {

  "Finding a Status by ID" should {
    "return each Status" in {
      Status(10).get must_== Created
      Status(20).get must_== Started
      Status(25).get must_== Aborted
      Status(30).get must_== Mate
      Status(31).get must_== Resign
      Status(32).get must_== Stalemate
      Status(33).get must_== Timeout
      Status(34).get must_== Draw
      Status(35).get must_== Outoftime
      Status(36).get must_== Cheat
      Status(37).get must_== NoStart
      Status(38).get must_== UnknownFinish
      Status(60).get must_== VariantEnd
    }

    "return None if the Status cannot be found" in {
      val invalidId = 142
      Status(invalidId) must beNone
    }
  }

  "Comparing two statuses" should {
    "return true if they are equal" in {
      Mate.is(Mate) must_== true
      Mate.is(_ => Mate) must_== true
    }

    "return false if they are not equal" in {
      Mate.is(Outoftime) must_== false
      Mate.is(_ => Aborted) must_== false
    }

    "return -1 if the ID of the first precedes the ID of the second" in {
      Timeout compare Cheat must_== -1
    }

    "return 0 if the two statuses are equal" in {
      Timeout compare Timeout must_== 0
    }

    "return 1 if the ID of the first succeeds the ID of the second" in {
      Timeout compare Started must_== 1
    }
  }
}
