package chess
package glicko

import munit.ScalaCheckSuite

class GlickoCalculatorTest extends ScalaCheckSuite with MunitExtensions:

  val calc = GlickoCalculator:
    Config(
      ratingPeriodsPerDay = impl.RatingPeriodsPerDay(0.21436d)
    )

  def computeGame(players: ByColor[Player], outcome: Outcome) =
    calc.computeGame(Game(players, outcome), skipDeviationIncrease = true).get.toPair

  {
    val players = ByColor.fill:
      Player(
        rating = 1500d,
        ratingDeviation = 500d,
        volatility = 0.09d,
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      )
    test("default deviation: white wins"):
      val (w, b) = computeGame(players, Outcome.white)
      assertCloseTo(w.rating, 1741d, 1d)
      assertCloseTo(b.rating, 1258d, 1d)
      assertCloseTo(w.ratingDeviation, 396d, 1d)
      assertCloseTo(b.ratingDeviation, 396d, 1d)
      assertCloseTo(w.volatility, 0.0899983, 0.00000001d)
      assertCloseTo(b.volatility, 0.0899983, 0.0000001d)
    test("default deviation: black wins"):
      val (w, b) = computeGame(players, Outcome.black)
      assertCloseTo(w.rating, 1258d, 1d)
      assertCloseTo(b.rating, 1741d, 1d)
      assertCloseTo(w.ratingDeviation, 396d, 1d)
      assertCloseTo(b.ratingDeviation, 396d, 1d)
      assertCloseTo(w.volatility, 0.0899983, 0.00000001d)
      assertCloseTo(b.volatility, 0.0899983, 0.0000001d)
    test("default deviation: draw"):
      val (w, b) = computeGame(players, Outcome.draw)
      assertCloseTo(w.rating, 1500d, 1d)
      assertCloseTo(b.rating, 1500d, 1d)
      assertCloseTo(w.ratingDeviation, 396d, 1d)
      assertCloseTo(b.ratingDeviation, 396d, 1d)
      assertCloseTo(w.volatility, 0.0899954, 0.0000001d)
      assertCloseTo(b.volatility, 0.0899954, 0.0000001d)
  }

  {
    val players = ByColor(
      Player(
        rating = 1400d,
        ratingDeviation = 79d,
        volatility = 0.06d,
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      ),
      Player(
        rating = 1550d,
        ratingDeviation = 110d,
        volatility = 0.065d,
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      )
    )
    test("mixed ratings and deviations: white wins"):
      val (w, b) = computeGame(players, Outcome.white)
      assertCloseTo(w.rating, 1422d, 1d)
      assertCloseTo(b.rating, 1506d, 1d)
      assertCloseTo(w.ratingDeviation, 77d, 1d)
      assertCloseTo(b.ratingDeviation, 105d, 1d)
      assertCloseTo(w.volatility, 0.06, 0.00001d)
      assertCloseTo(b.volatility, 0.065, 0.00001d)
    test("mixed ratings and deviations: black wins"):
      val (w, b) = computeGame(players, Outcome.black)
      assertCloseTo(w.rating, 1389d, 1d)
      assertCloseTo(b.rating, 1568d, 1d)
      assertCloseTo(w.ratingDeviation, 78d, 1d)
      assertCloseTo(b.ratingDeviation, 105d, 1d)
      assertCloseTo(w.volatility, 0.06, 0.00001d)
      assertCloseTo(b.volatility, 0.065, 0.00001d)
    test("mixed ratings and deviations: draw"):
      val (w, b) = computeGame(players, Outcome.draw)
      assertCloseTo(w.rating, 1406d, 1d)
      assertCloseTo(b.rating, 1537d, 1d)
      assertCloseTo(w.ratingDeviation, 78d, 1d)
      assertCloseTo(b.ratingDeviation, 105.87d, 0.01d)
      assertCloseTo(w.volatility, 0.06, 0.00001d)
      assertCloseTo(b.volatility, 0.065, 0.00001d)
  }

  {
    val players = ByColor(
      Player(
        rating = 1200d,
        ratingDeviation = 60d,
        volatility = 0.053d,
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      ),
      Player(
        rating = 1850d,
        ratingDeviation = 200d,
        volatility = 0.062d,
        numberOfResults = 0,
        lastRatingPeriodEnd = None
      )
    )
    test("more mixed ratings and deviations: white wins"):
      val (w, b) = computeGame(players, Outcome.white)
      assertCloseTo(w.rating, 1216.7d, 0.1d)
      assertCloseTo(b.rating, 1636d, 0.1d)
      assertCloseTo(w.ratingDeviation, 59.9d, 0.1d)
      assertCloseTo(b.ratingDeviation, 196.9d, 0.1d)
      assertCloseTo(w.volatility, 0.053013, 0.000001d)
      assertCloseTo(b.volatility, 0.062028, 0.000001d)
    test("more mixed ratings and deviations: black wins"):
      val (w, b) = computeGame(players, Outcome.black)
      assertCloseTo(w.rating, 1199.3d, 0.1d)
      assertCloseTo(b.rating, 1855.4d, 0.1d)
      assertCloseTo(w.ratingDeviation, 59.9d, 0.1d)
      assertCloseTo(b.ratingDeviation, 196.9d, 0.1d)
      assertCloseTo(w.volatility, 0.052999, 0.000001d)
      assertCloseTo(b.volatility, 0.061999, 0.000001d)
    test("more mixed ratings and deviations: draw"):
      val (w, b) = computeGame(players, Outcome.draw)
      assertCloseTo(w.rating, 1208.0, 0.1d)
      assertCloseTo(b.rating, 1745.7, 0.1d)
      assertCloseTo(w.ratingDeviation, 59.90056, 0.1d)
      assertCloseTo(b.ratingDeviation, 196.98729, 0.1d)
      assertCloseTo(w.volatility, 0.053002, 0.000001d)
      assertCloseTo(b.volatility, 0.062006, 0.000001d)
  }
