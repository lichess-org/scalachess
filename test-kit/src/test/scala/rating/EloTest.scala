package chess
package rating

class EloTest extends ChessTest:

  import Outcome.Points.*

  private def ratingDiff(
      r: Int,
      k: Int,
      opRating: Int,
      points: Outcome.Points,
      fideCat: FideTC,
      expected: Int
  )(using
      munit.Location
  ) =
    val player = Elo.Player(Elo(r), KFactor(k))
    val game = Elo.Game(points, Elo(opRating))
    assertEquals(Elo.computeRatingDiff(fideCat)(player, List(game)), expected)

  private def ratingDiffStandard(r: Int, k: Int, opRating: Int, points: Outcome.Points, expected: Int)(using
      munit.Location
  ) = ratingDiff(r, k, opRating, points, FideTC.standard, expected)

  test("new rating calculation over one game"):
    ratingDiffStandard(1500, 40, 1500, One, 20)
    ratingDiffStandard(1500, 40, 1500, Zero, -20)
    ratingDiffStandard(1500, 40, 1500, Half, 0)
    ratingDiffStandard(1500, 40, 1900, One, 37)
    ratingDiffStandard(1500, 40, 1900, Zero, -3)
    ratingDiffStandard(1500, 40, 1900, Half, 17)
    ratingDiffStandard(1500, 40, 2900, One, 37)
    ratingDiffStandard(1500, 40, 2900, Zero, -3)
    ratingDiffStandard(1500, 40, 2900, Half, 17)
    ratingDiffStandard(1500, 40, 1600, One, 26)
    ratingDiffStandard(1500, 40, 1600, Zero, -14)
    ratingDiffStandard(1500, 40, 1600, Half, 6)
    ratingDiffStandard(2000, 40, 1600, One, 3)
    ratingDiffStandard(2000, 40, 1600, Zero, -37)
    ratingDiffStandard(2000, 40, 1600, Half, -17)
    ratingDiffStandard(2000, 40, 1000, One, 3)
    ratingDiffStandard(2000, 40, 1000, Zero, -37)
    ratingDiffStandard(2000, 40, 1000, Half, -17)
    ratingDiffStandard(2000, 40, 1900, One, 14)
    ratingDiffStandard(2000, 40, 1900, Zero, -26)
    ratingDiffStandard(2000, 40, 1900, Half, -6)
    ratingDiffStandard(2800, 10, 1800, One, 0)

  test("new rating calculation over multiple games"):
    assertEquals(
      Elo.computeRatingDiff(FideTC.standard)(
        Elo.Player(Elo(2800), KFactor(10)),
        List.fill(11)(Elo.Game(One, Elo(1800)))
      ),
      0
    )

  test("new rating calculation rapid/blitz"):
    ratingDiff(1800, 40, 2601, Zero, FideTC.rapid, 0)
    ratingDiff(2601, 10, 1800, One, FideTC.rapid, 0)
    ratingDiff(2600, 10, 1900, One, FideTC.rapid, 1)
    ratingDiff(1500, 40, 1500, One, FideTC.rapid, 20)
    ratingDiff(1500, 40, 1900, Half, FideTC.blitz, 17)

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
    expectedScore(800, 0f)
    expectedScore(-800, 1f)

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
