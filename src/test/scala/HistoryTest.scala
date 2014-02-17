package chess

import com.roundeights.hasher.Implicits._
import Pos._

class HistoryTest extends ChessTest {

  "threefold repetition" should {
    def toHash(a: Int) = Array(a.toByte, 0.toByte)
    def makeHistory(positions: List[Int]) = (positions map toHash).foldLeft(History()) {
      case (history, hash) => history.copy(positionHashes = history.positionHashesWith(hash))
    }
    "empty history" in {
      History().threefoldRepetition must_== false
    }
    "not 3 same elements" in {
      val history = makeHistory(List(1, 2, 3, 4, 5, 2, 5, 6, 23, 55))
      history.threefoldRepetition must_== false
    }
    "not 3 elements same to the last one" in {
      val history = makeHistory(List(1, 2, 3, 4, 5, 2, 5, 6, 23, 2, 55))
      history.threefoldRepetition must_== false
    }
    "positive" in {
      val history = makeHistory(List(1, 2, 3, 4, 5, 2, 5, 6, 23, 2))
      history.threefoldRepetition must_== true
    }
  }
}
