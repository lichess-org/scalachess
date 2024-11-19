package chess
package glicko.impl

import munit.ScalaCheckSuite
import cats.syntax.all.*

class RatingCalculatorTest extends ScalaCheckSuite with MunitExtensions:

  // Chosen so a typical player's RD goes from 60 -> 110 in 1 year
  val ratingPeriodsPerDay = RatingPeriodsPerDay(0.21436d)

  val calculator = RatingCalculator(Tau.default, ratingPeriodsPerDay)

  def updateRatings(wRating: Rating, bRating: Rating, outcome: Outcome) =
    val results = GameRatingPeriodResults:
      List:
        outcome.winner match
          case None        => GameResult(wRating, bRating, true)
          case Some(White) => GameResult(wRating, bRating, false)
          case Some(Black) => GameResult(bRating, wRating, false)
    calculator.updateRatings(results, true)

  def defaultRating = Rating(
    rating = 1500d,
    ratingDeviation = 500d,
    volatility = 0.09d,
    numberOfResults = 0,
    lastRatingPeriodEnd = None
  )

  test("default deviation: white wins"):
    val wr = defaultRating
    val br = defaultRating
    updateRatings(wr, br, Outcome.white)
    assertCloseTo(wr.rating, 1741d, 1d)
    assertCloseTo(br.rating, 1258d, 1d)
    assertCloseTo(wr.ratingDeviation, 396d, 1d)
    assertCloseTo(br.ratingDeviation, 396d, 1d)
    assertCloseTo(wr.volatility, 0.0899983, 0.00000001d)
    assertCloseTo(br.volatility, 0.0899983, 0.0000001d)
  test("default deviation: black wins"):
    val wr = defaultRating
    val br = defaultRating
    updateRatings(wr, br, Outcome.black)
    assertCloseTo(wr.rating, 1258d, 1d)
    assertCloseTo(br.rating, 1741d, 1d)
    assertCloseTo(wr.ratingDeviation, 396d, 1d)
    assertCloseTo(br.ratingDeviation, 396d, 1d)
    assertCloseTo(wr.volatility, 0.0899983, 0.00000001d)
    assertCloseTo(br.volatility, 0.0899983, 0.0000001d)
  test("default deviation: draw"):
    val wr = defaultRating
    val br = defaultRating
    updateRatings(wr, br, Outcome.draw)
    assertCloseTo(wr.rating, 1500d, 1d)
    assertCloseTo(br.rating, 1500d, 1d)
    assertCloseTo(wr.ratingDeviation, 396d, 1d)
    assertCloseTo(br.ratingDeviation, 396d, 1d)
    assertCloseTo(wr.volatility, 0.0899954, 0.0000001d)
    assertCloseTo(br.volatility, 0.0899954, 0.0000001d)

  def oldRating = Rating(
    rating = 1500d,
    ratingDeviation = 80d,
    volatility = 0.06d,
    numberOfResults = 0,
    lastRatingPeriodEnd = None
  )
  test("low deviation: white wins"):
    val wr = oldRating
    val br = oldRating
    updateRatings(wr, br, Outcome.white)
    assertCloseTo(wr.rating, 1517d, 1d)
    assertCloseTo(br.rating, 1482d, 1d)
    assertCloseTo(wr.ratingDeviation, 78d, 1d)
    assertCloseTo(br.ratingDeviation, 78d, 1d)
    assertCloseTo(wr.volatility, 0.06, 0.00001d)
    assertCloseTo(br.volatility, 0.06, 0.00001d)
  test("low deviation: black wins"):
    val wr = oldRating
    val br = oldRating
    updateRatings(wr, br, Outcome.black)
    assertCloseTo(wr.rating, 1482d, 1d)
    assertCloseTo(br.rating, 1517d, 1d)
    assertCloseTo(wr.ratingDeviation, 78d, 1d)
    assertCloseTo(br.ratingDeviation, 78d, 1d)
    assertCloseTo(wr.volatility, 0.06, 0.00001d)
    assertCloseTo(br.volatility, 0.06, 0.00001d)
  test("low deviation: draw"):
    val wr = oldRating
    val br = oldRating
    updateRatings(wr, br, Outcome.draw)
    assertCloseTo(wr.rating, 1500d, 1d)
    assertCloseTo(br.rating, 1500d, 1d)
    assertCloseTo(wr.ratingDeviation, 78d, 1d)
    assertCloseTo(br.ratingDeviation, 78d, 1d)
    assertCloseTo(wr.volatility, 0.06, 0.00001d)
    assertCloseTo(br.volatility, 0.06, 0.00001d)
  {
    def whiteRating = Rating(
      rating = 1400d,
      ratingDeviation = 79d,
      volatility = 0.06d,
      numberOfResults = 0,
      lastRatingPeriodEnd = None
    )
    def blackRating = Rating(
      rating = 1550d,
      ratingDeviation = 110d,
      volatility = 0.065d,
      numberOfResults = 0,
      lastRatingPeriodEnd = None
    )
    test("mixed ratings and deviations: white wins"):
      val wr = whiteRating
      val br = blackRating
      updateRatings(wr, br, Outcome.white)
      assertCloseTo(wr.rating, 1422d, 1d)
      assertCloseTo(br.rating, 1506d, 1d)
      assertCloseTo(wr.ratingDeviation, 77d, 1d)
      assertCloseTo(br.ratingDeviation, 105d, 1d)
      assertCloseTo(wr.volatility, 0.06, 0.00001d)
      assertCloseTo(br.volatility, 0.065, 0.00001d)
    test("mixed ratings and deviations: black wins"):
      val wr = whiteRating
      val br = blackRating
      updateRatings(wr, br, Outcome.black)
      assertCloseTo(wr.rating, 1389d, 1d)
      assertCloseTo(br.rating, 1568d, 1d)
      assertCloseTo(wr.ratingDeviation, 78d, 1d)
      assertCloseTo(br.ratingDeviation, 105d, 1d)
      assertCloseTo(wr.volatility, 0.06, 0.00001d)
      assertCloseTo(br.volatility, 0.065, 0.00001d)
    test("mixed ratings and deviations: draw"):
      val wr = whiteRating
      val br = blackRating
      updateRatings(wr, br, Outcome.draw)
      assertCloseTo(wr.rating, 1406d, 1d)
      assertCloseTo(br.rating, 1537d, 1d)
      assertCloseTo(wr.ratingDeviation, 78d, 1d)
      assertCloseTo(br.ratingDeviation, 105.87d, 0.01d)
      assertCloseTo(wr.volatility, 0.06, 0.00001d)
      assertCloseTo(br.volatility, 0.065, 0.00001d)
  }
  {
    def whiteRating = Rating(
      rating = 1200d,
      ratingDeviation = 60d,
      volatility = 0.053d,
      numberOfResults = 0,
      lastRatingPeriodEnd = None
    )
    def blackRating = Rating(
      rating = 1850d,
      ratingDeviation = 200d,
      volatility = 0.062d,
      numberOfResults = 0,
      lastRatingPeriodEnd = None
    )
    test("more mixed ratings and deviations: white wins"):
      val wr = whiteRating
      val br = blackRating
      updateRatings(wr, br, Outcome.white)
      assertCloseTo(wr.rating, 1216.7d, 0.1d)
      assertCloseTo(br.rating, 1636d, 0.1d)
      assertCloseTo(wr.ratingDeviation, 59.9d, 0.1d)
      assertCloseTo(br.ratingDeviation, 196.9d, 0.1d)
      assertCloseTo(wr.volatility, 0.053013, 0.000001d)
      assertCloseTo(br.volatility, 0.062028, 0.000001d)
    test("more mixed ratings and deviations: black wins"):
      val wr = whiteRating
      val br = blackRating
      updateRatings(wr, br, Outcome.black)
      assertCloseTo(wr.rating, 1199.3d, 0.1d)
      assertCloseTo(br.rating, 1855.4d, 0.1d)
      assertCloseTo(wr.ratingDeviation, 59.9d, 0.1d)
      assertCloseTo(br.ratingDeviation, 196.9d, 0.1d)
      assertCloseTo(wr.volatility, 0.052999, 0.000001d)
      assertCloseTo(br.volatility, 0.061999, 0.000001d)
    test("more mixed ratings and deviations: draw"):
      val wr = whiteRating
      val br = blackRating
      updateRatings(wr, br, Outcome.draw)
      assertCloseTo(wr.rating, 1208.0, 0.1d)
      assertCloseTo(br.rating, 1745.7, 0.1d)
      assertCloseTo(wr.ratingDeviation, 59.90056, 0.1d)
      assertCloseTo(br.ratingDeviation, 196.98729, 0.1d)
      assertCloseTo(wr.volatility, 0.053002, 0.000001d)
      assertCloseTo(br.volatility, 0.062006, 0.000001d)
  }
