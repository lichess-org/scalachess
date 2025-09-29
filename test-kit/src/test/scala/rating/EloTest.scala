package chess
package rating

class EloTest extends ChessTest:

  import Outcome.Points.*

  private def ratingDiff(r: Int, k: Int, opRating: Int, points: Outcome.Points, expected: Int)(using
      munit.Location
  ) =
    val player = Elo.Player(Elo(r), KFactor(k))
    val game = Elo.Game(points, Elo(opRating))
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
    ratingDiff(2800, 10, 1800, One, 0)

  test("new rating calculation over multiple games"):
    assertEquals(
      Elo.computeRatingDiff(Elo.Player(Elo(2800), KFactor(10)), List.fill(11)(Elo.Game(One, Elo(1800)))),
      1
    )

  private def expectedScore(ratingDiff: Int, expScore: Float)(using munit.Location) =
    assertCloseTo(Elo.getExpectedScore(ratingDiff), expScore, 0.001f)

  test("expected score"):
    expectedScore(0, 0.5f)
    expectedScore(100, 0.36f)
    expectedScore(-100, 0.64f)
    expectedScore(300, 0.15f)
    expectedScore(-300, 0.85f)
    expectedScore(400, 0.08f)
    expectedScore(-400, 0.92f)
    expectedScore(800, 0.01f)
    expectedScore(-800, 0.99f)

  private def perfRating(games: Seq[Elo.Game], expected: Int)(using munit.Location) =
    assertEquals(Elo.computePerformanceRating(games), Some(Elo(expected)))

  test("performance rating"):
    def win(r: Int) = Elo.Game(One, Elo(r))
    def loss(r: Int) = Elo.Game(Zero, Elo(r))
    def draw(r: Int) = Elo.Game(Half, Elo(r))
    assertEquals(Elo.computePerformanceRating(Nil), None)
    perfRating(List(win(1500)), 2300)
    perfRating(List(win(1500), win(1500)), 2300)
    perfRating(List(loss(1500)), 700)
    perfRating(List(draw(1500)), 1500)
    perfRating(List(draw(1500), draw(1500)), 1500)
    perfRating(List(win(1500), loss(1500)), 1500)
    perfRating(List(win(1500), loss(1700)), 1600)
    perfRating(List(win(1500), draw(1500)), 1693)
    perfRating(List(win(1500), draw(1500), draw(1500)), 1625)
