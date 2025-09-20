package chess.rating.glicko

import chess.{ ByColor, Outcome }
import munit.ScalaCheckSuite

class GlickoCalculatorTest extends ScalaCheckSuite with chess.MunitExtensions:
  // Validate results with reference implementations
  // http://www.glicko.net/glicko/glicko2.pdf
  val R: Double = 1500d
  val RD: Double = 350d
  val V: Double = 0.06d

  val calc = GlickoCalculator(
    ratingPeriodsPerDay = RatingPeriodsPerDay(0.21436d)
  )

  def computeGame(players: ByColor[Player], outcome: Outcome) =
    calc.computeGame(Game(players, outcome), skipDeviationIncrease = true).get.toPair

  {
    val players = ByColor.fill:
      Player(
        Glicko(rating = R, deviation = RD, volatility = V)
      )
    test("default deviation: white wins"):
      val (w, b) = computeGame(players, Outcome.white)
      assertCloseTo(w.rating, 1662.21d, 0.005d)
      assertCloseTo(b.rating, 1337.79d, 0.005d)
      assertCloseTo(w.deviation, 290.23d, 0.005d)
      assertCloseTo(b.deviation, 290.23d, 0.005d)
      assertCloseTo(w.volatility, 0.0599993d, 0.0000001d)
      assertCloseTo(b.volatility, 0.0599993d, 0.0000001d)
    test("default deviation: black wins"):
      val (w, b) = computeGame(players, Outcome.black)
      assertCloseTo(w.rating, 1337.79d, 0.005d)
      assertCloseTo(b.rating, 1662.21d, 0.005d)
      assertCloseTo(w.deviation, 290.23d, 0.005d)
      assertCloseTo(b.deviation, 290.23d, 0.005d)
      assertCloseTo(w.volatility, 0.0599993d, 0.0000001d)
      assertCloseTo(b.volatility, 0.0599993d, 0.0000001d)
    test("default deviation: draw"):
      val (w, b) = computeGame(players, Outcome.draw)
      assertCloseTo(w.rating, 1500d, 0.005d)
      assertCloseTo(b.rating, 1500d, 0.005d)
      assertCloseTo(w.deviation, 290.23d, 0.005d)
      assertCloseTo(b.deviation, 290.23d, 0.005d)
      assertCloseTo(w.volatility, 0.0599977d, 0.0000001d)
      assertCloseTo(b.volatility, 0.0599977d, 0.0000001d)
  }

  {
    val players = ByColor(
      Player(
        Glicko(rating = 1400d, deviation = 79d, volatility = 0.06d),
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      ),
      Player(
        Glicko(rating = 1550d, deviation = 110d, volatility = 0.065d),
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      )
    )
    test("mixed ratings and deviations: white wins"):
      val (w, b) = computeGame(players, Outcome.white)
      assertCloseTo(w.rating, 1422.63d, 0.005d)
      assertCloseTo(b.rating, 1506.32d, 0.005d)
      assertCloseTo(w.deviation, 77.50d, 0.005d)
      assertCloseTo(b.deviation, 105.87d, 0.005d)
      assertCloseTo(w.volatility, 0.06, 0.00001d)
      assertCloseTo(b.volatility, 0.065, 0.00001d)
    test("mixed ratings and deviations: black wins"):
      val (w, b) = computeGame(players, Outcome.black)
      assertCloseTo(w.rating, 1389.99d, 0.005d)
      assertCloseTo(b.rating, 1568.90d, 0.005d)
      assertCloseTo(w.deviation, 77.50d, 0.005d)
      assertCloseTo(b.deviation, 105.87d, 0.005d)
      assertCloseTo(w.volatility, 0.06, 0.00001d)
      assertCloseTo(b.volatility, 0.065, 0.00001d)
    test("mixed ratings and deviations: draw"):
      val (w, b) = computeGame(players, Outcome.draw)
      assertCloseTo(w.rating, 1406.31d, 0.005d)
      assertCloseTo(b.rating, 1537.61d, 0.005d)
      assertCloseTo(w.deviation, 77.50d, 0.005d)
      assertCloseTo(b.deviation, 105.87d, 0.005d)
      assertCloseTo(w.volatility, 0.06, 0.00001d)
      assertCloseTo(b.volatility, 0.065, 0.00001d)
  }

  {
    val players = ByColor(
      Player(
        Glicko(rating = 1200d, deviation = 60d, volatility = 0.053d),
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      ),
      Player(
        Glicko(rating = 1850d, deviation = 200d, volatility = 0.062d),
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      )
    )
    test("more mixed ratings and deviations: white wins"):
      val (w, b) = computeGame(players, Outcome.white)
      assertCloseTo(w.rating, 1216.73d, 0.005d)
      assertCloseTo(b.rating, 1635.99d, 0.005d)
      assertCloseTo(w.deviation, 59.90d, 0.005d)
      assertCloseTo(b.deviation, 196.99d, 0.005d)
      assertCloseTo(w.volatility, 0.053013, 0.000001d)
      assertCloseTo(b.volatility, 0.062028, 0.000001d)
    test("more mixed ratings and deviations: black wins"):
      val (w, b) = computeGame(players, Outcome.black)
      assertCloseTo(w.rating, 1199.29d, 0.005d)
      assertCloseTo(b.rating, 1855.42d, 0.005d)
      assertCloseTo(w.deviation, 59.90d, 0.005d)
      assertCloseTo(b.deviation, 196.99d, 0.005d)
      assertCloseTo(w.volatility, 0.052999, 0.000001d)
      assertCloseTo(b.volatility, 0.061999, 0.000001d)
    test("more mixed ratings and deviations: draw"):
      val (w, b) = computeGame(players, Outcome.draw)
      assertCloseTo(w.rating, 1208.01d, 0.005d)
      assertCloseTo(b.rating, 1745.71d, 0.005d)
      assertCloseTo(w.deviation, 59.90056, 0.005d)
      assertCloseTo(b.deviation, 196.98729, 0.005d)
      assertCloseTo(w.volatility, 0.053002, 0.000001d)
      assertCloseTo(b.volatility, 0.062006, 0.000001d)
  }
