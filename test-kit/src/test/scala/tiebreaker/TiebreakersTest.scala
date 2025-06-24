package chess

import chess.tiebreakers.Tiebreaker.*
import chess.rating.Elo
import chess.Outcome.Points

class TiebreakersTest extends ChessTest:

  val playerA = Player("PlayerA", rating = Elo(1500))
  val playerB = Player("PlayerB", rating = Elo(1600))
  val playerC = Player("PlayerC", rating = Elo(1550))
  val playerD = Player("PlayerD", rating = Elo(1450))
  val playerE = Player("PlayerE", rating = Elo(1650))

  case class Game(white: Player, black: Player, result: ByColor[Points])

  // Crosstable:
  //          | A   B   C   D   E   | Total
  // ---------------------------------------
  // PlayerA  | X   1   ½   0   1   | 2.5
  // PlayerB  | 0   X   1   ½   0   | 1.5
  // PlayerC  | ½   0   X   1   ½   | 2.0
  // PlayerD  | 1   ½   0   X   1   | 2.5
  // PlayerE  | 0   1   ½   0   X   | 1.5

  // Define all games based on crosstable
  val games = Seq(
    Game(playerA, playerB, ByColor(Points.One, Points.Zero)),  // A beats B
    Game(playerA, playerC, ByColor(Points.Half, Points.Half)), // A draws C
    Game(playerD, playerA, ByColor(Points.One, Points.Zero)),  // D beats A
    Game(playerA, playerE, ByColor(Points.One, Points.Zero)),  // A beats E

    Game(playerB, playerC, ByColor(Points.One, Points.Zero)),  // B beats C
    Game(playerB, playerD, ByColor(Points.Half, Points.Half)), // B draws D
    Game(playerE, playerB, ByColor(Points.One, Points.Zero)),  // E beats B

    Game(playerC, playerD, ByColor(Points.One, Points.Zero)),  // C beats D
    Game(playerC, playerE, ByColor(Points.Half, Points.Half)), // C draws E

    Game(playerD, playerE, ByColor(Points.One, Points.Zero)) // D beats E
  )

  def povGames(player: Player): Seq[POVGame] =
    games.collect:
      case Game(white, black, result) if white == player || black == player =>
        val playerColor = if white == player then Color.White else Color.Black
        POVGame(Some(result(playerColor)), if playerColor == Color.White then black else white, playerColor)

  val playerA_Games     = PlayerGames(playerA, povGames(playerA))
  val playerB_Games     = PlayerGames(playerB, povGames(playerB))
  val playerC_Games     = PlayerGames(playerC, povGames(playerC))
  val playerD_Games     = PlayerGames(playerD, povGames(playerD))
  val playerE_Games     = PlayerGames(playerE, povGames(playerE))
  val allGames          = Seq(playerA_Games, playerB_Games, playerC_Games, playerD_Games, playerE_Games)
  val playerA_opponents = Seq(playerB_Games, playerC_Games, playerD_Games, playerE_Games)

  test("scores"):
    assertEquals(playerA_Games.score, 2.5f)
    assertEquals(playerB_Games.score, 1.5f)
    assertEquals(playerC_Games.score, 2.0f)
    assertEquals(playerD_Games.score, 2.5f)
    assertEquals(playerE_Games.score, 1.5f)

  test("NbGames"):
    val tiebreaker = tb(NbGames, playerA, allGames)
    assertEquals(tiebreaker, 4.0f)

  test("NbBlackGames"):
    val tiebreaker = tb(NbBlackGames, playerA, allGames)
    assertEquals(tiebreaker, 1.0f)

  test("NbWins"):
    val tiebreaker = tb(NbWins, playerA, allGames)
    assertEquals(tiebreaker, 2.0f)

  test("NbBlackWins"):
    val tiebreaker = tb(NbBlackWins, playerA, allGames)
    assertEquals(tiebreaker, 0f)

  test("SonnebornBerger"):
    val tiebreaker = tb(SonnebornBerger, playerA, allGames)
    assertEquals(tiebreaker, 4.0f)

  test("SonnebornBergerCut1"):
    val tiebreaker = tb(SonnebornBergerCut1, playerA, allGames)
    assertEquals(tiebreaker, 4.0f)

  test("Buchholz"):
    val tiebreaker = tb(Buchholz, playerA, allGames)
    assertEquals(tiebreaker, 7.5f)

  test("BuchholzCut1"):
    val tiebreaker = tb(BuchholzCut1, playerA, allGames)
    assertEquals(tiebreaker, 6f)

  test("BuchholzCut2"):
    val tiebreaker = tb(BuchholzCut2, playerA, allGames)
    assertEquals(tiebreaker, 4.5f)

  test("AverageOfOpponentsBuchholz"):
    val tiebreaker = tb(AverageOfOpponentsBuchholz, playerA, allGames)
    assertEquals(tiebreaker, 8.125f)

  test("DirectEncounter"):
    val tiebreaker1 = tb(DirectEncounter, playerA, allGames)
    val tiebreaker2 = tb(
      DirectEncounter,
      playerD,
      allGames
    )
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 1f)

  test("AverageOpponentRating"):
    val tiebreaker = tb(AverageOpponentRating, playerA, allGames)
    assertEquals(tiebreaker, 1562.5f)
