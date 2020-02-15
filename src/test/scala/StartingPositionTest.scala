package chess

import chess.ChessTest
import chess.StartingPosition._
import chess.StartingPosition.presets._

class StartingPositionTest extends ChessTest {

  "StartingPosition" should {
    "return a URL for the position" in {
      halloween.url must_== "https://en.wikipedia.org/wiki/Halloween_Gambit"
    }

    "return a name for the position" in {
      halloween.name must_== "Halloween Gambit"
    }

    "return false if the position is not the initial board position" in {
      halloween.initial must_== false
    }

    "return true if the position is the initial board position" in {
      initial.initial must_== true
    }

    "return all positions, including the initial one" in {
      allWithInitial.length must beGreaterThan(1)
      allWithInitial must contain(initial)
    }

    "return a random featurable position" in {
      val featurablePosition = randomFeaturable
      featurablePosition.featurable must_== true
      allWithInitial must contain(featurablePosition)
    }
  }
}
