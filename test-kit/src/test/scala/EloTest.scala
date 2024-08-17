package chess

class EloTest extends ChessTest:

  private def newRating(r: Int, k: Int, winOpt: Option[Boolean], opRating: Int): Int =
    val player = Elo.Player(Elo(r), KFactor(k))
    val game   = Elo.Game(winOpt, Elo(opRating))
    Elo.computeNewRating(player, List(game)).value

  test("new rating calculation"):
    List(
      (1500, 40, 1500, Some(true), 1520),
      (1500, 40, 1500, Some(false), 1480),
      (1500, 40, 1500, None, 1500),
      (1500, 40, 1900, Some(true), 1536),
      (1500, 40, 1900, Some(false), 1496),
      (1500, 40, 1900, None, 1516),
      (1500, 40, 2000, Some(true), 1536),
      (1500, 40, 2000, Some(false), 1496),
      (1500, 40, 2000, None, 1516),
      (1500, 40, 1600, Some(true), 1525),
      (1500, 40, 1600, Some(false), 1485),
      (1500, 40, 1600, None, 1505)
    ).foreach: (rating, k, opRating, win, expected) =>
      assertEquals(newRating(rating, k, win, opRating), expected)
