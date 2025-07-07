package chess
package tiebreaker

import cats.syntax.all.*
import chess.Outcome.Points
import chess.rating.Elo
import chess.tiebreaker.*
import chess.tiebreaker.Tiebreaker.*

class TiebreakersTest extends ChessTest:

  val playerA = Player("PlayerA", rating = Elo(1500).some)
  val playerB = Player("PlayerB", rating = Elo(1600).some)
  val playerC = Player("PlayerC", rating = Elo(1550).some)
  val playerD = Player("PlayerD", rating = Elo(1450).some)
  val playerE = Player("PlayerE", rating = Elo(1650).some)

  case class TestGame(white: Player, black: Player, result: ByColor[Points])

  extension (p: Player)
    def beats(opponent: Player) = TestGame(p, opponent, ByColor(Points.One, Points.Zero))
    def draws(opponent: Player) = TestGame(p, opponent, ByColor(Points.Half, Points.Half))
    def loses(opponent: Player) = opponent.beats(p)

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
    playerA.beats(playerB),
    playerA.draws(playerC),
    playerA.loses(playerD),
    playerA.beats(playerE),
    playerB.beats(playerC),
    playerB.draws(playerD),
    playerB.loses(playerE),
    playerC.beats(playerD),
    playerC.draws(playerE),
    playerD.beats(playerE)
  )

  def povGames(player: Player): Seq[Game] =
    games.collect:
      case TestGame(white, black, result) if white == player || black == player =>
        val playerColor = Color.fromWhite(white == player)
        Game(Some(result(playerColor)), playerColor.fold(black, white), playerColor)

  val playerA_Games = PlayerWithGames(playerA, povGames(playerA))
  val playerB_Games = PlayerWithGames(playerB, povGames(playerB))
  val playerC_Games = PlayerWithGames(playerC, povGames(playerC))
  val playerD_Games = PlayerWithGames(playerD, povGames(playerD))
  val playerE_Games = PlayerWithGames(playerE, povGames(playerE))
  val allGames      = Seq(playerA_Games, playerB_Games, playerC_Games, playerD_Games, playerE_Games).mapBy(
    _.player.id
  )
  val playerA_opponents = Seq(playerB_Games, playerC_Games, playerD_Games, playerE_Games)

  test("scores"):
    assertEquals(playerA_Games.games.score, 2.5f)
    assertEquals(playerB_Games.games.score, 1.5f)
    assertEquals(playerC_Games.games.score, 2.0f)
    assertEquals(playerD_Games.games.score, 2.5f)
    assertEquals(playerE_Games.games.score, 1.5f)

  test("NbBlackGames"):
    val tiebreaker = NbBlackGames.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 1.0f)

  test("NbWins"):
    val tiebreaker = NbWins.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 2.0f)

  test("NbBlackWins"):
    val tiebreaker = NbBlackWins.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 0f)

  test("SonnebornBerger"):
    val tiebreaker = SonnebornBerger.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 4.0f)

  test("SonnebornBergerCut1"):
    val tiebreaker = SonnebornBergerCut1.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 4.0f)

  test("Buchholz"):
    val tiebreaker = Buchholz.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 7.5f)

  test("BuchholzCut1"):
    val tiebreaker = BuchholzCut1.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 6f)

  test("BuchholzCut2"):
    val tiebreaker = BuchholzCut2.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 4.5f)

  test("ForeBuchholz"):
    val tiebreaker = ForeBuchholz.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 8f)

  test("ForeBuchholzCut1"):
    val tiebreaker = ForeBuchholzCut1.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 6f)

  test("AverageOfOpponentsBuchholz"):
    val tiebreaker = AverageOfOpponentsBuchholz.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 8.125f)

  test("DirectEncounter"):
    val tiebreaker1 = DirectEncounter.compute(playerA, allGames, Map.empty)
    val tiebreaker2 = DirectEncounter.compute(playerD, allGames, Map.empty)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 1f)

  test("DirectEncounter with more than one game"):
    val extraDraw = Seq(
      playerA_Games.copy(games = playerA_Games.games ++ Seq(Game(Some(Points.Half), playerD, Color.White))),
      playerB_Games,
      playerC_Games,
      playerD_Games.copy(games = playerD_Games.games ++ Seq(Game(Some(Points.Half), playerA, Color.Black))),
      playerE_Games
    ).mapBy(_.player.id)
    val tiebreaker1 = DirectEncounter.compute(playerD, extraDraw, Map.empty)
    val tiebreaker2 = DirectEncounter.compute(playerA, extraDraw, Map.empty)
    assertEquals(tiebreaker1, 0.75f) // 1 win + 1 draw / 2 games
    assertEquals(tiebreaker2, 0.25f) // 1 draw / 2 games

  test("DirectEncounter with unequal partial tiebreaks"):
    val previousPoints = Map(
      playerA.id -> List(Point(NbBlackWins, TieBreakPoints(1f))),
      playerD.id -> List(Point(NbBlackWins, TieBreakPoints(0.5f)))
    )

    val tiebreaker1 = DirectEncounter.compute(playerA, allGames, previousPoints)
    val tiebreaker2 = DirectEncounter.compute(playerD, allGames, previousPoints)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 0f)

  test("DirectEncounter with equal partial tiebreaks"):

    val previousPoints = Map(
      playerA.id -> List(Point(NbBlackWins, TieBreakPoints(1f))),
      playerD.id -> List(Point(NbBlackWins, TieBreakPoints(1f)))
    )

    val tiebreaker1 = DirectEncounter.compute(playerA, allGames, previousPoints)
    val tiebreaker2 = DirectEncounter.compute(playerD, allGames, previousPoints)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 1f)

  test("DirectEncounter with equal partial tiebreaks but not all players have met"):

    // Create a player X that has not played against A or D
    // A, D and X are all on 2.5 points with partial tiebreaks of 1 but only A and D have met
    val playerX_Games = PlayerWithGames(
      Player("PlayerX", rating = Elo(1500).some),
      Seq(
        Game(Some(Points.Half), playerB, Color.White),
        Game(Some(Points.One), playerD, Color.White),
        Game(Some(Points.One), playerE, Color.Black)
      )
    )

    val previousPoints = Map(
      playerA.id              -> List(Point(NbBlackWins, TieBreakPoints(1f))),
      playerD.id              -> List(Point(NbBlackWins, TieBreakPoints(1f))),
      playerX_Games.player.id -> List(Point(NbBlackWins, TieBreakPoints(1f)))
    )

    val allGamesWithPartial =
      Seq(playerA_Games, playerD_Games, playerC_Games, playerD_Games, playerE_Games, playerX_Games)
        .mapBy(_.player.id)

    val tiebreaker1 = DirectEncounter.compute(playerA, allGamesWithPartial, previousPoints)
    val tiebreaker2 = DirectEncounter.compute(playerD, allGamesWithPartial, previousPoints)
    val tiebreakerX = DirectEncounter.compute(playerX_Games.player, allGamesWithPartial, previousPoints)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 0f)
    assertEquals(tiebreakerX, 0f)

  test("AverageOpponentRating"):
    val tiebreaker = AverageRatingOfOpponents.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 1563f)

  test("AverageOpponentRating with unrated opponents"):
    val unratedOpponent = Player("Unrated Opponent", rating = None)
    val unratedGames    = Seq(
      Game(Some(Points.One), unratedOpponent, Color.White)
    )
    val unratedPlayerGames = Seq(PlayerWithGames(unratedOpponent, unratedGames)).mapBy(_.player.id)
    val tiebreaker         = AverageRatingOfOpponents.compute(playerA, unratedPlayerGames, Map.empty)
    assertEquals(tiebreaker, 0f)
    assertEquals(AverageRatingOfOpponents.compute(playerA, allGames ++ unratedPlayerGames, Map.empty), 1563f)

  test("AveragePerformanceOfOpponents"):
    val tiebreaker = AveragePerformanceOfOpponents.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 1527f)

  test("TournamentPerformanceRating"):
    val tiebreaker = TournamentPerformanceRating.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 1657f)

  test("KoyaSystem"):
    val tiebreaker = KoyaSystem.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 0.5f)

  test("SumOfProgressiveScores"):
    val tiebreaker = SumOfProgressiveScores.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 6.5f)

  test("SumOfProgressiveScoresCut1"):
    val tiebreaker = SumOfProgressiveScoresCut1.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 5.5f)

  test("PerfectTournamentPerformance - Perfect scores"):
    // from https://chess-results.com/tnr1166026.aspx?lan=1&art=1&rd=8
    val ruslan = Player("Ruslan Pogorelov", rating = Elo(2255).some)
    val josep  = Player("Josep M. Beltran Reverter", rating = Elo(1834).some)
    val carles = Player("Carles Costas Bella", rating = Elo(1929).some)
    val sergi  = Player("Sergi Aubanell Ber", rating = Elo(1988).some)
    val xavier = Player("Xavier Palomo Teruel", rating = Elo(2145).some)
    val agusti = Player("Agusti Guasch Figuerola", rating = Elo(1990).some)
    val daniel = Player("Daniel Torrens Gonzalez", rating = Elo(1965).some)
    val aaron  = Player("Aaron Alfonso Pellisa", rating = Elo(2125).some)

    val ruslanGames = Seq(
      Game(Some(Points.One), josep, Color.White),
      Game(Some(Points.One), carles, Color.Black),
      Game(Some(Points.One), sergi, Color.White),
      Game(Some(Points.Half), Player("bye", None), Color.White),
      Game(Some(Points.One), xavier, Color.Black),
      Game(Some(Points.One), agusti, Color.White),
      Game(Some(Points.One), daniel, Color.Black),
      Game(Some(Points.One), aaron, Color.White)
    )
    val ruslanPlayerGames = Seq(PlayerWithGames(ruslan, ruslanGames)).mapBy(_.player.id)
    assertEquals(TournamentPerformanceRating.compute(ruslan, ruslanPlayerGames, Map.empty), 2796f)
    assertEquals(
      PerfectTournamentPerformance.compute(ruslan, ruslanPlayerGames, Map.empty),
      2945f
    ) // chess-results says 2949. Perfect scores though so :shrug:

  test("PerfectTournamentPerformance - Regular"):
    val marc     = Player("Marc Guardia Curto", rating = Elo(1830).some)
    val enric    = Player("Enric Regue Farran", rating = Elo(1914).some)
    val josepmg  = Player("Josep Maria Guasch Murtra", rating = Elo(1867).some)
    val francesc = Player("Francesc Xavier Senso Moreno", rating = Elo(1818).some)
    val ruslan   = Player("Ruslan Pogorelov", rating = Elo(2255).some)
    val agusti   = Player("Agusti Guasch Figuerola", rating = Elo(1990).some)
    val xavier   = Player("Xavier Palomo Teruel", rating = Elo(2145).some)

    val xavierGames = Seq(
      Game(Some(Points.One), marc, Color.White),
      Game(Some(Points.One), enric, Color.Black),
      Game(Some(Points.One), josepmg, Color.White),
      Game(Some(Points.Zero), ruslan, Color.White),
      Game(Some(Points.One), francesc, Color.Black),
      // POVGame(Some(Points.Zero), Player("bye", None), Color.White), // Rd 7: bye (0)
      Game(Some(Points.One), agusti, Color.White)
    )
    // Lila excludes all bye games. So we don't need to check for them.
    val xavierPlayerGames = Seq(PlayerWithGames(xavier, xavierGames)).mapBy(_.player.id)
    assertEquals(TournamentPerformanceRating.compute(xavier, xavierPlayerGames, Map.empty), 2218f)
    assertEquals(PerfectTournamentPerformance.compute(xavier, xavierPlayerGames, Map.empty), 2259f)

  test("PerfectTournamentPerformance - Zero score"):
    val games = Seq(
      Game(Some(Points.Zero), playerB, Color.Black),
      Game(Some(Points.Zero), playerC, Color.White),
      Game(Some(Points.Zero), playerD, Color.Black)
    )
    val playerGames = Seq(PlayerWithGames(playerA, games)).mapBy(_.player.id)
    // Lowest rated opponent - 800
    assertEquals(PerfectTournamentPerformance.compute(playerA, playerGames, Map.empty), 650f)

  test("AveragePerfectPerformanceOfOpponents"):
    val tiebreaker = AveragePerfectPerformanceOfOpponents.compute(playerA, allGames, Map.empty)
    assertEquals(tiebreaker, 1523f) // 1444 + 1549 + 1668 + 1432 = 6093 / 4 = 1523.25, rounded to 1523
