package chess

import scala.math.round

final class EloCalculator(inflation: Boolean = false) {

  // Player 1 wins
  val P1WIN = -1;

  // No player wins
  val DRAW = 0;

  // Player 2 wins
  val P2WIN = 1;

  type User = {
    def elo: Int
    def countRated: Int
  }

  def calculate(user1: User, user2: User, win: Option[Color]): (Int, Int) = {
    val winCode = win match {
      case None        ⇒ DRAW
      case Some(White) ⇒ P1WIN
      case Some(Black) ⇒ P2WIN
    }
    val (user1Elo, user2Elo) = (
      calculateUserElo(user1, user2.elo, -winCode), 
      calculateUserElo(user2, user1.elo, winCode)
    )

    inflation.fold(
      inflate(user1, user1Elo, user2, user2Elo), 
      (user1Elo, user2Elo)
    )
  }

  def diff(user1: User, user2: User, winner: Option[Color]): Int =
    (user1 == user2).fold(
      0,
      calculate(user1, user2, winner)._1 - user1.elo
    )

  private def inflate(user1: User, user1Elo: Int, user2: User, user2Elo: Int): (Int, Int) = {
    if (user1Elo > user1.elo) (user1Elo + 1, user2Elo)
    else if (user2Elo > user2.elo) (user1Elo, user2Elo + 1)
    else (user1Elo, user2Elo)
  }

  private def calculateUserElo(user: User, opponentElo: Int, win: Int) = {
    val score = (1 + win) / 2f
    val expected = 1 / (1 + math.pow(10, (opponentElo - user.elo) / 400f))
    val kFactor = math.round(
      if (user.countRated > 20) 16
      else 50 - user.countRated * (34 / 20f)
    )
    val diff = 2 * kFactor * (score - expected)

    round(user.elo + diff).toInt
  }
}
