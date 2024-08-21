package chess

class EloTest extends ChessTest:

  private def ratingDiff(r: Int, k: Int, opRating: Int, winOpt: Option[Boolean], expected: Int)(using
      munit.Location
  ) =
    val player = Elo.Player(Elo(r), KFactor(k))
    val game   = Elo.Game(winOpt, Elo(opRating))
    assertEquals(Elo.computeRatingDiff(player, List(game)), expected)

  test("new rating calculation over one game"):
    ratingDiff(1500, 40, 1500, Some(true), 20)
    ratingDiff(1500, 40, 1500, Some(false), -20)
    ratingDiff(1500, 40, 1500, None, 0)
    ratingDiff(1500, 40, 1900, Some(true), 36)
    ratingDiff(1500, 40, 1900, Some(false), -4)
    ratingDiff(1500, 40, 1900, None, 16)
    ratingDiff(1500, 40, 2900, Some(true), 36)
    ratingDiff(1500, 40, 2900, Some(false), -4)
    ratingDiff(1500, 40, 2900, None, 16)
    ratingDiff(1500, 40, 1600, Some(true), 26)
    ratingDiff(1500, 40, 1600, Some(false), -14)
    ratingDiff(1500, 40, 1600, None, 6)

  private def perfRating(games: Seq[Elo.Game], expected: Int)(using munit.Location) =
    assertEquals(Elo.computePerformanceRating(games), Some(Elo(expected)))

  test("performance rating"):
    def win(r: Int)  = Elo.Game(Some(true), Elo(r))
    def loss(r: Int) = Elo.Game(Some(false), Elo(r))
    def draw(r: Int) = Elo.Game(None, Elo(r))
    assertEquals(Elo.computePerformanceRating(Nil), None)
    perfRating(List(win(1500)), 1900)
    perfRating(List(win(1500), win(1500)), 1900)
    perfRating(List(loss(1500)), 1100)
    perfRating(List(draw(1500)), 1500)
    perfRating(List(draw(1500), draw(1500)), 1500)
    perfRating(List(win(1500), loss(1500)), 1500)
    perfRating(List(win(1500), draw(1500)), 1700)
