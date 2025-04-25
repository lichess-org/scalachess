package chess

import chess.Outcome.Points

class TiebreakersTest extends ChessTest:

  val game1 = POVGame(plies = Ply(40), points = Some(Points.One), opponent = "PlayerB", color = Color.White)
  val game2 = POVGame(plies = Ply(40), points = Some(Points.Half), opponent = "PlayerC", color = Color.Black)
  val game3 = POVGame(plies = Ply(20), points = Some(Points.Zero), opponent = "PlayerD", color = Color.Black)
  val game4 = POVGame(plies = Ply(1), points = Some(Points.One), opponent = "PlayerE", color = Color.White)

  val playerA = PlayerGames("PlayerA", Seq(game1, game2, game3, game4))
  val playerB = PlayerGames(
    "PlayerB",
    Seq(
      game1.copy(opponent = "PlayerA", points = Some(Points.Zero)),
      POVGame(plies = Ply(30), points = Some(Points.One), opponent = "PlayerC", color = Color.White),
      POVGame(plies = Ply(25), points = Some(Points.Half), opponent = "PlayerD", color = Color.Black),
      POVGame(plies = Ply(15), points = Some(Points.Zero), opponent = "PlayerE", color = Color.White)
    )
  )
  val playerC = PlayerGames(
    "PlayerC",
    Seq(
      game2.copy(opponent = "PlayerA", points = Some(Points.Half)),
      POVGame(plies = Ply(30), points = Some(Points.Zero), opponent = "PlayerB", color = Color.Black),
      POVGame(plies = Ply(20), points = Some(Points.One), opponent = "PlayerD", color = Color.White),
      POVGame(plies = Ply(10), points = Some(Points.Half), opponent = "PlayerE", color = Color.Black)
    )
  )
  val playerD = PlayerGames(
    "PlayerD",
    Seq(
      game3.copy(opponent = "PlayerA", points = Some(Points.One)),
      POVGame(plies = Ply(25), points = Some(Points.Half), opponent = "PlayerB", color = Color.White),
      POVGame(plies = Ply(20), points = Some(Points.Zero), opponent = "PlayerC", color = Color.Black),
      POVGame(plies = Ply(5), points = Some(Points.One), opponent = "PlayerE", color = Color.White)
    )
  )
  val playerE = PlayerGames(
    "PlayerE",
    Seq(
      game4.copy(opponent = "PlayerA", points = Some(Points.Zero)),
      POVGame(plies = Ply(15), points = Some(Points.One), opponent = "PlayerB", color = Color.Black),
      POVGame(plies = Ply(10), points = Some(Points.Half), opponent = "PlayerC", color = Color.White),
      POVGame(plies = Ply(5), points = Some(Points.Zero), opponent = "PlayerD", color = Color.Black)
    )
  )

  val opponents = Seq(playerB, playerC, playerD, playerE)

  // Crosstable:
  //          | A   B   C   D   E   | Total
  // ---------------------------------------
  // PlayerA  | X   1   ½   0   1   | 2.5
  // PlayerB  | 0   X   1   ½   0   | 1.5
  // PlayerC  | ½   0   X   1   ½   | 2.0
  // PlayerD  | 1   ½   0   X   1   | 2.5
  // PlayerE  | 0   1   ½   0   X   | 1.5

  test("NbGames"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.NbGames, playerA, opponents)
    assertEquals(tiebreaker, 4.0f)

  test("NbBlackGames"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.NbBlackGames, playerA, opponents)
    assertEquals(tiebreaker, 2.0f)

  test("NbWins"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.NbWins, playerA, opponents)
    assertEquals(tiebreaker, 2.0f)

  test("NbBlackWins"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.NbBlackWins, playerA, opponents)
    assertEquals(tiebreaker, 0f)

  test("NbWinsExcludingByes"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.NbWinsExcludingByes, playerA, opponents)
    assertEquals(tiebreaker, 1.0f) // Excludes game4 with plies <= 2

  test("SonnebornBerger"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.SonnebornBerger, playerA, opponents)
    assertEquals(tiebreaker, 4.0f)

  test("Buchholz"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.Buchholz, playerA, opponents)
    assertEquals(tiebreaker, opponents.map(_.score).sum)

  test("BuchholzCut1"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.BuchholzCut1, playerA, opponents)
    assertEquals(tiebreaker, opponents.map(_.score).sorted.drop(1).sum)

  test("BuchholzCut2"):
    val tiebreaker = Tiebreaker.tb(Tiebreaker.BuchholzCut2, playerA, opponents)
    assertEquals(tiebreaker, opponents.map(_.score).sorted.drop(2).sum)

  test("DirectEncounter"):
    val tiebreaker1 = Tiebreaker.tb(Tiebreaker.DirectEncounter, playerA, opponents)
    val tiebreaker2 = Tiebreaker.tb(Tiebreaker.DirectEncounter, playerD, Seq(playerA, playerB, playerC, playerE))
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 1f)
