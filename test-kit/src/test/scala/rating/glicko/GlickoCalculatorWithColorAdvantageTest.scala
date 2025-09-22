package chess.rating.glicko

import chess.{ ByColor, Outcome }
import munit.ScalaCheckSuite

class GlickoCalculatorWithColorAdvantageTest extends ScalaCheckSuite with chess.MunitExtensions:
  // Validate results with reference implementations
  // http://www.glicko.net/glicko/glicko2.pdf
  val R: Double = 1500d
  val RD: Double = 350d
  val V: Double = 0.06d

  val calc = GlickoCalculator(
    ratingPeriodsPerDay = RatingPeriodsPerDay(0.21436d),
    colorAdvantage = ColorAdvantage.zero
  )

  val calcWithAdvantage = GlickoCalculator(
    ratingPeriodsPerDay = RatingPeriodsPerDay(0.21436d),
    colorAdvantage = ColorAdvantage.standard
  )

  val calcWithMoreAdvantage = GlickoCalculator(
    ratingPeriodsPerDay = RatingPeriodsPerDay(0.21436d),
    colorAdvantage = ColorAdvantage.crazyhouse
  )

  def computeGame(players: ByColor[Player], outcome: Outcome) =
    calc.computeGame(Game(players, outcome), skipDeviationIncrease = true).get.toPair

  def computeGameWithAdvantage(players: ByColor[Player], outcome: Outcome) =
    calcWithAdvantage.computeGame(Game(players, outcome), skipDeviationIncrease = true).get.toPair

  def computeGameWithMoreAdvantage(players: ByColor[Player], outcome: Outcome) =
    calcWithMoreAdvantage.computeGame(Game(players, outcome), skipDeviationIncrease = true).get.toPair

  {
    val players = ByColor.fill:
      Player(
        Glicko(rating = R, deviation = RD, volatility = V)
      )
    test("default deviation: white wins"):
      val (w1, b1) = computeGame(players, Outcome.white)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.white)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.white)
      assertCloseTo(w1.rating, 1662.21d, 0.005d)
      assertCloseTo(b1.rating, 1337.79d, 0.005d)
      // outcome was equally probable for both players
      assert(w1.deviation == b1.deviation)
      assertCloseTo(w1.deviation, 290.2305d, 0.00005d)
      assertCloseTo(b1.deviation, 290.2305d, 0.00005d)
      // outcome was equally probable for both players
      assert(w1.volatility == b1.volatility)
      assertCloseTo(w1.volatility, 0.0599993d, 0.0000001d)
      assertCloseTo(b1.volatility, 0.0599993d, 0.0000001d)
      assert(w2.deviation == b2.deviation)
      assertCloseTo(w2.deviation, 290.2407d, 0.00005d)
      assertCloseTo(b2.deviation, 290.2407d, 0.00005d)
      assert(w2.volatility == b2.volatility)
      assertCloseTo(w2.volatility, 0.0599993d, 0.0000001d)
      assertCloseTo(b2.volatility, 0.0599993d, 0.0000001d)
      assert(w3.deviation == b3.deviation)
      assertCloseTo(w3.deviation, 290.2692d, 0.00005d)
      assertCloseTo(b3.deviation, 290.2692d, 0.00005d)
      assert(w3.volatility == b3.volatility)
      assertCloseTo(w3.volatility, 0.0599992d, 0.0000001d)
      assertCloseTo(b3.volatility, 0.0599992d, 0.0000001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      // white win is more expected (first move advantage)
      assert(w1.volatility > w2.volatility)
      assert(b1.volatility > b2.volatility)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
      // white win is more expected (crazyhouse first move)
      assert(w2.volatility > w3.volatility)
      assert(b2.volatility > b3.volatility)
    test("default deviation: black wins"):
      val (w1, b1) = computeGame(players, Outcome.black)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.black)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.black)
      assertCloseTo(w1.rating, 1337.79d, 0.005d)
      assertCloseTo(b1.rating, 1662.21d, 0.005d)
      assert(w1.deviation == b1.deviation)
      assertCloseTo(w1.deviation, 290.2305d, 0.00005d)
      assertCloseTo(b1.deviation, 290.2305d, 0.00005d)
      assert(w1.volatility == b1.volatility)
      assertCloseTo(w1.volatility, 0.0599993d, 0.0000001d)
      assertCloseTo(b1.volatility, 0.0599993d, 0.0000001d)
      assert(w2.deviation == b2.deviation)
      assertCloseTo(w2.deviation, 290.2407d, 0.00005d)
      assertCloseTo(b2.deviation, 290.2407d, 0.00005d)
      assert(w2.volatility == b2.volatility)
      assertCloseTo(w2.volatility, 0.0599993d, 0.0000001d)
      assertCloseTo(b2.volatility, 0.0599993d, 0.0000001d)
      assert(w3.deviation == b3.deviation)
      assertCloseTo(w3.deviation, 290.2692d, 0.00005d)
      assertCloseTo(b3.deviation, 290.2692d, 0.00005d)
      assert(w3.volatility == b3.volatility)
      assertCloseTo(w3.volatility, 0.0599993d, 0.0000001d)
      assertCloseTo(b3.volatility, 0.0599993d, 0.0000001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      // black win is less expected (first move advantage)
      assert(w1.volatility < w2.volatility)
      assert(b1.volatility < b2.volatility)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
      // black win is less expected (crazyhouse first move)
      assert(w2.volatility < w3.volatility)
      assert(b2.volatility < b3.volatility)
    test("default deviation: draw"):
      val (w1, b1) = computeGame(players, Outcome.draw)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.draw)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.draw)
      assertCloseTo(w1.rating, 1500d, 0.005d)
      assertCloseTo(b1.rating, 1500d, 0.005d)
      assert(w1.deviation == b1.deviation)
      assertCloseTo(w1.deviation, 290.2305d, 0.00005d)
      assertCloseTo(b1.deviation, 290.2305d, 0.00005d)
      assert(w1.volatility == b1.volatility)
      assertCloseTo(w1.volatility, 0.0599977d, 0.0000001d)
      assertCloseTo(b1.volatility, 0.0599977d, 0.0000001d)
      assert(w2.deviation == b2.deviation)
      assertCloseTo(w2.deviation, 290.2407d, 0.00005d)
      assertCloseTo(b2.deviation, 290.2407d, 0.00005d)
      assert(w2.volatility == b2.volatility)
      assertCloseTo(w2.volatility, 0.0599977d, 0.0000001d)
      assertCloseTo(b2.volatility, 0.0599977d, 0.0000001d)
      assert(w3.deviation == b3.deviation)
      assertCloseTo(w3.deviation, 290.2692d, 0.00005d)
      assertCloseTo(b3.deviation, 290.2692d, 0.00005d)
      assert(w3.volatility == b3.volatility)
      assertCloseTo(w3.volatility, 0.0599977d, 0.0000001d)
      assertCloseTo(b3.volatility, 0.0599977d, 0.0000001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      // draw is less expected (first move advantage)
      assert(w1.volatility < w2.volatility)
      assert(b1.volatility < b2.volatility)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
      // draw is less expected (crazyhouse first move)
      assert(w2.volatility < w3.volatility)
      assert(b2.volatility < b3.volatility)
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
      val (w1, b1) = computeGame(players, Outcome.white)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.white)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.white)
      assertCloseTo(w1.rating, 1422.63d, 0.005d)
      assertCloseTo(b1.rating, 1506.32d, 0.005d)
      assertCloseTo(w1.deviation, 77.4956d, 0.00005d)
      assertCloseTo(b1.deviation, 105.8706d, 0.00005d)
      assertCloseTo(w1.volatility, 0.06d, 0.00001d)
      assertCloseTo(b1.volatility, 0.065d, 0.00001d)
      assertCloseTo(w2.deviation, 77.4721d, 0.00005d)
      assertCloseTo(b2.deviation, 105.8046d, 0.00005d)
      assertCloseTo(w2.volatility, 0.06d, 0.00001d)
      assertCloseTo(b2.volatility, 0.065d, 0.00001d)
      assertCloseTo(w3.deviation, 77.4505d, 0.00005d)
      assertCloseTo(b3.deviation, 105.7440d, 0.00005d)
      assertCloseTo(w3.volatility, 0.06d, 0.00001d)
      assertCloseTo(b3.volatility, 0.065d, 0.00001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
    test("mixed ratings and deviations: black wins"):
      val (w1, b1) = computeGame(players, Outcome.black)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.black)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.black)
      assertCloseTo(w1.rating, 1389.99d, 0.005d)
      assertCloseTo(b1.rating, 1568.90d, 0.005d)
      assertCloseTo(w1.deviation, 77.4956d, 0.00005d)
      assertCloseTo(b1.deviation, 105.8706d, 0.00005d)
      assertCloseTo(w1.volatility, 0.06d, 0.00001d)
      assertCloseTo(b1.volatility, 0.065d, 0.00001d)
      assertCloseTo(w2.deviation, 77.4721d, 0.00005d)
      assertCloseTo(b2.deviation, 105.8046d, 0.00005d)
      assertCloseTo(w2.volatility, 0.06d, 0.00001d)
      assertCloseTo(b2.volatility, 0.065d, 0.00001d)
      assertCloseTo(w3.deviation, 77.4505d, 0.00005d)
      assertCloseTo(b3.deviation, 105.744d, 0.00005d)
      assertCloseTo(w3.volatility, 0.06d, 0.00001d)
      assertCloseTo(b3.volatility, 0.065d, 0.00001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
    test("mixed ratings and deviations: draw"):
      val (w1, b1) = computeGame(players, Outcome.draw)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.draw)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.draw)
      assertCloseTo(w1.rating, 1406.31d, 0.005d)
      assertCloseTo(b1.rating, 1537.61d, 0.005d)
      assertCloseTo(w1.deviation, 77.4956d, 0.00005d)
      assertCloseTo(b1.deviation, 105.8706d, 0.00005d)
      assertCloseTo(w1.volatility, 0.06d, 0.00001d)
      assertCloseTo(b1.volatility, 0.065d, 0.00001d)
      assertCloseTo(w2.deviation, 77.4721d, 0.00005d)
      assertCloseTo(b2.deviation, 105.8046d, 0.00005d)
      assertCloseTo(w2.volatility, 0.06d, 0.00001d)
      assertCloseTo(b2.volatility, 0.065d, 0.00001d)
      assertCloseTo(w3.deviation, 77.4505d, 0.00005d)
      assertCloseTo(b3.deviation, 105.7440d, 0.00005d)
      assertCloseTo(w3.volatility, 0.06d, 0.00001d)
      assertCloseTo(b3.volatility, 0.065d, 0.00001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
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
      val (w1, b1) = computeGame(players, Outcome.white)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.white)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.white)
      assertCloseTo(w1.rating, 1216.73d, 0.005d)
      assertCloseTo(b1.rating, 1635.99d, 0.005d)
      assertCloseTo(w1.deviation, 59.9006d, 0.00005d)
      assertCloseTo(b1.deviation, 196.9873d, 0.00005d)
      assertCloseTo(w1.volatility, 0.053013d, 0.000001d)
      assertCloseTo(b1.volatility, 0.062028d, 0.000001d)
      assertCloseTo(w2.deviation, 59.8971d, 0.00005d)
      assertCloseTo(b2.deviation, 196.8617d, 0.00005d)
      assertCloseTo(w2.volatility, 0.053013d, 0.000001d)
      assertCloseTo(b2.volatility, 0.062028d, 0.000001d)
      assertCloseTo(w3.deviation, 59.8936d, 0.00005d)
      assertCloseTo(b3.deviation, 196.7381d, 0.00005d)
      assertCloseTo(w3.volatility, 0.053013d, 0.000001d)
      assertCloseTo(b3.volatility, 0.062028d, 0.000001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
    test("more mixed ratings and deviations: black wins"):
      val (w1, b1) = computeGame(players, Outcome.black)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.black)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.black)
      assertCloseTo(w1.rating, 1199.29d, 0.005d)
      assertCloseTo(b1.rating, 1855.42d, 0.005d)
      assertCloseTo(w1.deviation, 59.9006d, 0.00005d)
      assertCloseTo(b1.deviation, 196.9873d, 0.00005d)
      assertCloseTo(w1.volatility, 0.052999d, 0.000001d)
      assertCloseTo(b1.volatility, 0.061999d, 0.000001d)
      assertCloseTo(w2.deviation, 59.8971d, 0.00005d)
      assertCloseTo(b2.deviation, 196.8617d, 0.00005d)
      assertCloseTo(w2.volatility, 0.052999d, 0.000001d)
      assertCloseTo(b2.volatility, 0.061999d, 0.000001d)
      assertCloseTo(w3.deviation, 59.8936d, 0.00005d)
      assertCloseTo(b3.deviation, 196.7381d, 0.00005d)
      assertCloseTo(w3.volatility, 0.052999d, 0.000001d)
      assertCloseTo(b3.volatility, 0.061999d, 0.000001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
    test("more mixed ratings and deviations: draw"):
      val (w1, b1) = computeGame(players, Outcome.draw)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.draw)
      val (w3, b3) = computeGameWithMoreAdvantage(players, Outcome.draw)
      assertCloseTo(w1.rating, 1208.01d, 0.005d)
      assertCloseTo(b1.rating, 1745.71d, 0.005d)
      assertCloseTo(w1.deviation, 59.90056d, 0.00005d)
      assertCloseTo(b1.deviation, 196.98729d, 0.00005d)
      assertCloseTo(w1.volatility, 0.053002d, 0.000001d)
      assertCloseTo(b1.volatility, 0.062006d, 0.000001d)
      assertCloseTo(w2.deviation, 59.8971d, 0.00005d)
      assertCloseTo(b2.deviation, 196.8617d, 0.00005d)
      assertCloseTo(w2.volatility, 0.053002d, 0.000001d)
      assertCloseTo(b2.volatility, 0.062006d, 0.000001d)
      assertCloseTo(w3.deviation, 59.8936d, 0.00005d)
      assertCloseTo(b3.deviation, 196.7381d, 0.00005d)
      assertCloseTo(w3.volatility, 0.053002d, 0.000001d)
      assertCloseTo(b3.volatility, 0.062006d, 0.000001d)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
      assert(w2.rating > w3.rating)
      assert(b2.rating < b3.rating)
  }
