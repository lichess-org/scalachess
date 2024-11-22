package chess
package rating.glicko

import cats.syntax.all.*
import munit.ScalaCheckSuite

class RatingCalculatorWithColorAdvantageTest extends ScalaCheckSuite with chess.MunitExtensions:

  val smallAdvantage = GlickoCalculator(
    Tau.default,
    RatingPeriodsPerDay.default,
    ColorAdvantage(7d)
  )

  // test that the rating calculator correctly applies the color advantage
  import Outcome.*
  // http://www.glicko.net/glicko/glicko2.pdf
  // If the player is unrated, set the rating to 1500
  // and the RD to 350. Set the player’s volatility to
  // 0.06 (this value depends on the particular application)
  val RD: Double = 350d
  val V: Double = 0.06d

  private def ratingDiff(r: Int, opRating: Int, outcome: Outcome, expected: Int)(using
      munit.Location
  ) =
    val player = Player(Glicko(r, RD, V))
    val opponent = Player(Glicko(opRating, RD, V))
    val game   = Game(ByColor(player, opponent), outcome)
    // TODO: calculator with color advantage
    val calculator = GlickoCalculator()
    calculator.computeGame(game)

  test("new rating calculation over one game"):
    ratingDiff(1500, 1500, white, 20)
    ratingDiff(1500, 1500, black, -20)
    ratingDiff(1500, 1500, draw, 0)
    ratingDiff(1500, 1900, white, 37)
    ratingDiff(1500, 1900, black, -3)
    ratingDiff(1500, 1900, draw, 17)
    ratingDiff(1500, 2900, white, 37)
    ratingDiff(1500, 2900, black, -3)
    ratingDiff(1500, 2900, draw, 17)
    ratingDiff(1500, 1600, white, 26)
    ratingDiff(1500, 1600, black, -14)
    ratingDiff(1500, 1600, draw, 6)
    ratingDiff(2000, 1600, white, 3)
    ratingDiff(2000, 1600, black, -37)
    ratingDiff(2000, 1600, draw, -17)
    ratingDiff(2000, 1000, white, 3)
    ratingDiff(2000, 1000, black, -37)
    ratingDiff(2000, 1000, draw, -17)
    ratingDiff(2000, 1900, white, 14)
    ratingDiff(2000, 1900, black, -26)
    ratingDiff(2000, 1900, draw, -6)
