package chess

class EloTest extends ChessTest:

  import Outcome.Points.*

  private def ratingDiff(r: Int, k: Int, opRating: Int, points: Outcome.Points, expected: Int)(using
      munit.Location
  ) =
    val player = Elo.Player(Elo(r), KFactor(k))
    val game   = Elo.Game(points, Elo(opRating))
    assertEquals(Elo.computeRatingDiff(player, List(game)), expected)

  test("new rating calculation over one game"):
    ratingDiff(1500, 40, 1500, One, 20)
    ratingDiff(1500, 40, 1500, Zero, -20)
    ratingDiff(1500, 40, 1500, Half, 0)
    ratingDiff(1500, 40, 1900, One, 37)
    ratingDiff(1500, 40, 1900, Zero, -3)
    ratingDiff(1500, 40, 1900, Half, 17)
    ratingDiff(1500, 40, 2900, One, 37)
    ratingDiff(1500, 40, 2900, Zero, -3)
    ratingDiff(1500, 40, 2900, Half, 17)
    ratingDiff(1500, 40, 1600, One, 26)
    ratingDiff(1500, 40, 1600, Zero, -14)
    ratingDiff(1500, 40, 1600, Half, 6)
    ratingDiff(2000, 40, 1600, One, 3)
    ratingDiff(2000, 40, 1600, Zero, -37)
    ratingDiff(2000, 40, 1600, Half, -17)
    ratingDiff(2000, 40, 1000, One, 3)
    ratingDiff(2000, 40, 1000, Zero, -37)
    ratingDiff(2000, 40, 1000, Half, -17)
    ratingDiff(2000, 40, 1900, One, 14)
    ratingDiff(2000, 40, 1900, Zero, -26)
    ratingDiff(2000, 40, 1900, Half, -6)

  private def expectedScore(ratingDiff: Int, expScore: BigDecimal)(using munit.Location) =
    assertEquals(Elo.getExpectedScore(ratingDiff), expScore)

  test("expected score"):
    expectedScore(0, 0.5)
    expectedScore(100, 0.36)
    expectedScore(-100, 0.64)
    expectedScore(300, 0.15)
    expectedScore(-300, 0.85)
    expectedScore(400, 0.08)
    expectedScore(-400, 0.92)
    expectedScore(800, 0.08)
    expectedScore(-800, 0.92)

  private def perfRating(games: Seq[Elo.Game], expected: Int)(using munit.Location) =
    assertEquals(Elo.computePerformanceRating(games), Some(Elo(expected)))

  test("performance rating"):
    def win(r: Int)  = Elo.Game(One, Elo(r))
    def loss(r: Int) = Elo.Game(Zero, Elo(r))
    def draw(r: Int) = Elo.Game(Half, Elo(r))
    assertEquals(Elo.computePerformanceRating(Nil), None)
    perfRating(List(win(1500)), 1900)
    perfRating(List(win(1500), win(1500)), 1900)
    perfRating(List(loss(1500)), 1100)
    perfRating(List(draw(1500)), 1500)
    perfRating(List(draw(1500), draw(1500)), 1500)
    perfRating(List(win(1500), loss(1500)), 1500)
    perfRating(List(win(1500), draw(1500)), 1700)
