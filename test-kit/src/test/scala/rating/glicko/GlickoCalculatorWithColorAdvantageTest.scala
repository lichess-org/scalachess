package chess.rating.glicko

import chess.{ ByColor, Outcome }
import munit.ScalaCheckSuite

class GlickoCalculatorWithColorAdvantageTest extends ScalaCheckSuite with chess.MunitExtensions:
  // Validate results with reference implementations
  // http://www.glicko.net/glicko/glicko2.pdf
  val R: Double  = 1500d
  val RD: Double = 350d
  val V: Double  = 0.06d

  val calc = GlickoCalculator(
    ratingPeriodsPerDay = RatingPeriodsPerDay(0.21436d),
    colorAdvantage = ColorAdvantage.zero
  )

  val calcWithAdvantage = GlickoCalculator(
    ratingPeriodsPerDay = RatingPeriodsPerDay(0.21436d),
    colorAdvantage = ColorAdvantage.standard
  )

  def computeGame(players: ByColor[Player], outcome: Outcome) =
    calc.computeGame(Game(players, outcome), skipDeviationIncrease = true).get.toPair

  def computeGameWithAdvantage(players: ByColor[Player], outcome: Outcome) =
    calcWithAdvantage.computeGame(Game(players, outcome), skipDeviationIncrease = true).get.toPair

  {
    val players = ByColor.fill:
      Player(
        Glicko(rating = R, deviation = RD, volatility = V)
      )
    test("default deviation: white wins"):
      val (w1, b1) = computeGame(players, Outcome.white)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.white)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
    test("default deviation: black wins"):
      val (w1, b1) = computeGame(players, Outcome.black)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.black)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
    test("default deviation: draw"):
      val (w1, b1) = computeGame(players, Outcome.draw)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.draw)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
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
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
    test("mixed ratings and deviations: black wins"):
      val (w1, b1) = computeGame(players, Outcome.black)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.black)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
    test("mixed ratings and deviations: draw"):
      val (w1, b1) = computeGame(players, Outcome.draw)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.draw)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
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
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
    test("more mixed ratings and deviations: black wins"):
      val (w1, b1) = computeGame(players, Outcome.black)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.black)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
    test("more mixed ratings and deviations: draw"):
      val (w1, b1) = computeGame(players, Outcome.draw)
      val (w2, b2) = computeGameWithAdvantage(players, Outcome.draw)
      assert(w1.rating > w2.rating)
      assert(b1.rating < b2.rating)
  }
