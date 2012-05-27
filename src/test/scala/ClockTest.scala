package chess

import Pos._

class ClockTest extends ChessTest {

  "play with a clock" should {
    val clock = Clock(5 * 60 * 1000, 0)
    val game = Game() withClock clock
    "new game" in {
      game.clock must beSome.like {
        case c ⇒ c.color must_== White
      }
    }
    "one move played" in {
      game.playMoves(E2 -> E4) must beSuccess.like {
        case g ⇒ g.clock must beSome.like {
          case c ⇒ c.color must_== Black
        }
      }
    }
  }
  "create a clock" should {
    "with time" in {
      Clock(60, 10).limit must_== 60
    }
    "with increment" in {
      Clock(60, 10).increment must_== 10
    }
    "with few time" in {
      Clock(0, 10).limit must_== 2
    }
  }
}
