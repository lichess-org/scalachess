package chess.rating.glicko

import cats.syntax.all.*
import munit.ScalaCheckSuite

class RatingCalculatorWithColorAdvantageTest extends ScalaCheckSuite with chess.MunitExtensions:

  val smallAdvantage = GlickoCalculator(
    Tau.default,
    RatingPeriodsPerDay.default,
    ColorAdvantage(7d)
  )

  // test that the rating calculator correctly applies the color advantage
