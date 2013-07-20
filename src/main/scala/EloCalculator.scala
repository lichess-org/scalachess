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

  def calculate(white: User, black: User, win: Option[Color]): (Int, Int) = {
    val winCode = win match {
      case None        ⇒ DRAW
      case Some(White) ⇒ P1WIN
      case Some(Black) ⇒ P2WIN
    }
    val (whiteElo, blackElo) = (
      calculateUserElo(white, black.elo, -winCode), 
      calculateUserElo(black, white.elo, winCode)
    )

    inflation.fold(
      inflate(white, whiteElo, black, blackElo), 
      (whiteElo, blackElo)
    )
  }

  def diff(white: User, black: User, winner: Option[Color]): Int =
    (white == black).fold(
      0,
      calculate(white, black, winner)._1 - white.elo
    )

  private def inflate(white: User, whiteElo: Int, black: User, blackElo: Int): (Int, Int) = {
    if (whiteElo > white.elo) (whiteElo + 1, blackElo)
    else if (blackElo > black.elo) (whiteElo, blackElo + 1)
    else (whiteElo, blackElo)
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
