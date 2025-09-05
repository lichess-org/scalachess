package chess
package tiebreak

import cats.syntax.all.*
import chess.Outcome.Points
import chess.rating.Elo
import chess.tiebreak.*
import chess.tiebreak.Tiebreak.*

class TiebreakTest extends ChessTest:

  val playerA = Player("1", "PlayerA".some, rating = Elo(1500).some)
  val playerB = Player("2", "PlayerB".some, rating = Elo(1600).some)
  val playerC = Player("3", "PlayerC".some, rating = Elo(1550).some)
  val playerD = Player("4", "PlayerD".some, rating = Elo(1450).some)
  val playerE = Player("5", "PlayerE".some, rating = Elo(1650).some)

  case class TestGame(white: Player, black: Player, result: ByColor[Points], roundId: Option[String])

  extension (p: Player)
    def beats(opponent: Player, roundId: String) =
      TestGame(p, opponent, ByColor(Points.One, Points.Zero), roundId.some)
    def draws(opponent: Player, roundId: String) =
      TestGame(p, opponent, ByColor(Points.Half, Points.Half), roundId.some)
    def loses(opponent: Player, roundId: String) = opponent.beats(p, roundId)

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
    playerA.beats(playerB, "1"),
    playerA.draws(playerC, "2"),
    playerA.loses(playerD, "3"),
    playerA.beats(playerE, "4"),
    playerB.beats(playerC, "1"),
    playerB.draws(playerD, "2"),
    playerB.loses(playerE, "3"),
    playerC.beats(playerD, "1"),
    playerC.draws(playerE, "2"),
    playerD.beats(playerE, "1")
  )

  def povGames(player: Player): Seq[Game] =
    games.collect:
      case TestGame(white, black, result, roundId) if white == player || black == player =>
        val playerColor = Color.fromWhite(white == player)
        Game(result(playerColor), playerColor.fold(black, white), playerColor, roundId)

  val playerA_Games = PlayerWithGames(playerA, povGames(playerA))
  val playerB_Games = PlayerWithGames(playerB, povGames(playerB))
  val playerC_Games = PlayerWithGames(playerC, povGames(playerC))
  val playerD_Games = PlayerWithGames(playerD, povGames(playerD))
  val playerE_Games = PlayerWithGames(playerE, povGames(playerE))
  val allGames =
    Seq(playerA_Games, playerB_Games, playerC_Games, playerD_Games, playerE_Games).mapBy(_.player.id)

  def computeTournamentPoints(
      allGames: Map[PlayerId, PlayerWithGames],
      player: Player,
      tiebreak: Tiebreak
  ): Option[TiebreakPoint] =
    Tournament(allGames)
      .compute(List(tiebreak))
      .find(_.player.id == player.id)
      .flatMap(_.tiebreakPoints.headOption)

  test("scores"):
    assertEquals(playerA_Games.games.score, 2.5f)
    assertEquals(playerB_Games.games.score, 1.5f)
    assertEquals(playerC_Games.games.score, 2.0f)
    assertEquals(playerD_Games.games.score, 2.5f)
    assertEquals(playerE_Games.games.score, 1.5f)

  test("NbBlackGames"):
    val points = computeTournamentPoints(allGames, playerA, NbBlackGames)
    assertEquals(points, Some(TiebreakPoint(1.0f)))

  test("NbWins"):
    val points = computeTournamentPoints(allGames, playerA, NbWins)
    assertEquals(points, Some(TiebreakPoint(2.0f)))

  test("NbBlackWins"):
    val points = computeTournamentPoints(allGames, playerA, NbBlackWins)
    assertEquals(points, Some(TiebreakPoint(0f)))

  test("SonnebornBerger"):
    val points = computeTournamentPoints(allGames, playerA, SonnebornBerger(CutModifier.None))
    assertEquals(points, Some(TiebreakPoint(4.0f)))

  test("SonnebornBergerCut1"):
    val points =
      computeTournamentPoints(allGames, playerA, SonnebornBerger(modifier = CutModifier.Cut1))
    assertEquals(points, Some(TiebreakPoint(4.0f)))

  test("SonnebornBergerMedian1"):
    val points =
      computeTournamentPoints(allGames, playerA, SonnebornBerger(modifier = CutModifier.Median1))
    assertEquals(points, Some(TiebreakPoint(2.5f)))

  test("Buchholz"):
    val points = computeTournamentPoints(allGames, playerA, Buchholz(CutModifier.None))
    assertEquals(points, Some(TiebreakPoint(7.5f)))

  test("BuchholzCut1"):
    val points = computeTournamentPoints(allGames, playerA, Buchholz(CutModifier.Cut1))
    assertEquals(points, Some(TiebreakPoint(6f)))

  test("BuchholzCut2"):
    val points = computeTournamentPoints(allGames, playerA, Buchholz(CutModifier.Cut2))
    assertEquals(points, Some(TiebreakPoint(4.5f)))

  test("BuchholzMedian1"):
    val points = computeTournamentPoints(allGames, playerA, Buchholz(CutModifier.Median1))
    assertEquals(points, Some(TiebreakPoint(3.5f)))

  test("BuchholzMedian2"):
    val points = computeTournamentPoints(allGames, playerA, Buchholz(CutModifier.Median2))
    assertEquals(points, Some(TiebreakPoint(0f)))

  test("ForeBuchholz"):
    val points = computeTournamentPoints(allGames, playerA, ForeBuchholz(CutModifier.None))
    assertEquals(points, Some(TiebreakPoint(8f)))

  test("ForeBuchholz with one game played in last round"):
    val playerB_GamesWithLastRound = playerB_Games.copy(
      games = playerB_Games.games ++ Seq(Game(Points.Zero, playerA, Color.White, "5".some))
    )
    val allGamesWithLastRound = allGames.updated(playerB.id, playerB_GamesWithLastRound)
    val points = computeTournamentPoints(allGamesWithLastRound, playerA, ForeBuchholz(CutModifier.None))
    // PlayerB's points should be 2. The rest don't get the +-0.5 from the last round
    assertEquals(points, Some(TiebreakPoint(8f)))

  test("ForeBuchholzCut1"):
    val points = computeTournamentPoints(allGames, playerA, ForeBuchholz(CutModifier.Cut1))
    assertEquals(points, Some(TiebreakPoint(6f)))

  test("ForeBuchholzMedian1"):
    val points = computeTournamentPoints(allGames, playerA, ForeBuchholz(CutModifier.Median1))
    assertEquals(points, Some(TiebreakPoint(4f)))

  test("ForeBuchholzMedian2"):
    val points = computeTournamentPoints(allGames, playerA, ForeBuchholz(CutModifier.Median2))
    assertEquals(points, Some(TiebreakPoint(0f)))

  test("AverageOfOpponentsBuchholz"):
    val points = computeTournamentPoints(allGames, playerA, AverageOfOpponentsBuchholz)
    assertEquals(points, Some(TiebreakPoint(8.125f)))

  test("DirectEncounter"):
    val points1 = computeTournamentPoints(allGames, playerA, DirectEncounter)
    val points2 = computeTournamentPoints(allGames, playerD, DirectEncounter)
    assertEquals(points1, Some(TiebreakPoint(0f)))
    assertEquals(points2, Some(TiebreakPoint(1f)))

  test("DirectEncounter with more than one game"):
    val extraDraw = Seq(
      playerA_Games.copy(games =
        playerA_Games.games ++ Seq(Game(Points.Half, playerD, Color.White, "6".some))
      ),
      playerB_Games,
      playerC_Games,
      playerD_Games.copy(games =
        playerD_Games.games ++ Seq(Game(Points.Half, playerA, Color.Black, "6".some))
      ),
      playerE_Games
    ).mapBy(_.player.id)
    val points1 = computeTournamentPoints(extraDraw, playerD, DirectEncounter)
    val points2 = computeTournamentPoints(extraDraw, playerA, DirectEncounter)
    assertEquals(points1, Some(TiebreakPoint(0.75f)))
    assertEquals(points2, Some(TiebreakPoint(0.25f)))

  test("DirectEncounter with unequal partial tiebreaks"):
    val previousPoints = Map(
      playerA.id -> List(TiebreakPoint(1f)),
      playerD.id -> List(TiebreakPoint(0.5f))
    )

    val points1 = DirectEncounter
      .compute(Tournament(allGames), previousPoints)
      .get(playerA.id)
      .flatMap(_.lift(1))
    val points2 = DirectEncounter
      .compute(Tournament(allGames), previousPoints)
      .get(playerD.id)
      .flatMap(_.lift(1))
    assertEquals(points1, Some(TiebreakPoint(0f)))
    assertEquals(points2, Some(TiebreakPoint(0f)))

  test("DirectEncounter with equal partial tiebreaks"):

    val previousPoints = Map(
      playerA.id -> List(TiebreakPoint(1f)),
      playerD.id -> List(TiebreakPoint(1f))
    )

    val points1 = DirectEncounter
      .compute(Tournament(allGames), previousPoints)
      .get(playerA.id)
      .flatMap(_.lift(1))
    val points2 = DirectEncounter
      .compute(Tournament(allGames), previousPoints)
      .get(playerD.id)
      .flatMap(_.lift(1))
    assertEquals(points1, Some(TiebreakPoint(0f)))
    assertEquals(points2, Some(TiebreakPoint(1f)))

  test("DirectEncounter with equal partial tiebreaks but not all players have met"):

    // Create a player X that has not played against A or D
    // A, D and X are all on 2.5 points with partial tiebreaks of 1 but only A and D have met
    val playerX_Games = PlayerWithGames(
      Player("99", "PlayerX".some, rating = Elo(1500).some),
      Seq(
        Game(Points.Half, playerB, Color.White, "1".some),
        Game(Points.One, playerD, Color.White, "2".some),
        Game(Points.One, playerE, Color.Black, "3".some)
      )
    )

    val previousPoints = Map(
      playerA.id -> List(TiebreakPoint(1f)),
      playerD.id -> List(TiebreakPoint(1f)),
      playerX_Games.player.id -> List(TiebreakPoint(1f))
    )

    val allGamesWithPartial =
      Seq(playerA_Games, playerD_Games, playerC_Games, playerD_Games, playerE_Games, playerX_Games)
        .mapBy(_.player.id)

    val points1 = DirectEncounter
      .compute(Tournament(allGamesWithPartial), previousPoints)
      .get(playerA.id)
      .flatMap(_.lift(1))
    val points2 = DirectEncounter
      .compute(Tournament(allGamesWithPartial), previousPoints)
      .get(playerD.id)
      .flatMap(_.lift(1))
    val pointsX = DirectEncounter
      .compute(Tournament(allGamesWithPartial), previousPoints)
      .get(playerX_Games.player.id)
      .flatMap(_.lift(1))
    assertEquals(points1, Some(TiebreakPoint(0f)))
    assertEquals(points2, Some(TiebreakPoint(0f)))
    assertEquals(pointsX, Some(TiebreakPoint(0f)))

  test("AverageOpponentRating"):
    val points = computeTournamentPoints(allGames, playerA, AverageRatingOfOpponents(CutModifier.None))
    assertEquals(points, Some(TiebreakPoint(1563f)))

  test("AverageOpponentRatingCut1"):
    val points = computeTournamentPoints(allGames, playerA, AverageRatingOfOpponents(CutModifier.Cut1))
    assertEquals(points, Some(TiebreakPoint(1600f)))

  test("AverageOpponentRatingMedian1"):
    val points =
      computeTournamentPoints(allGames, playerA, AverageRatingOfOpponents(CutModifier.Median1))
    assertEquals(points, Some(TiebreakPoint(1575f)))

  test("AverageOpponentRatingMedian2"):
    val points =
      computeTournamentPoints(allGames, playerA, AverageRatingOfOpponents(CutModifier.Median2))
    assertEquals(points, Some(TiebreakPoint(0f)))

  test("AverageOpponentRating with unrated opponents"):
    val unratedOpponent = Player("100", "Unrated Opponent".some, rating = None)
    val unratedGames = Seq(
      Game(Points.One, unratedOpponent, Color.White, "1".some)
    )
    val unratedPlayerGames = Seq(PlayerWithGames(playerA, unratedGames))
    val points =
      computeTournamentPoints(
        unratedPlayerGames.mapBy(_.player.id),
        playerA,
        AverageRatingOfOpponents(CutModifier.None)
      )
    assertEquals(points, Some(TiebreakPoint(0f)))
    assertEquals(
      computeTournamentPoints(
        allGames.updated(playerA.id, playerA_Games.copy(games = playerA_Games.games ++ unratedGames)),
        playerA,
        AverageRatingOfOpponents(CutModifier.None)
      ),
      Some(TiebreakPoint(1563f))
    )

  test("AveragePerformanceOfOpponents"):
    val points = computeTournamentPoints(allGames, playerA, AveragePerformanceOfOpponents)
    assertEquals(points, Some(TiebreakPoint(1527f)))

  test("TournamentPerformanceRating"):
    val points = computeTournamentPoints(allGames, playerA, TournamentPerformanceRating)
    assertEquals(points, Some(TiebreakPoint(1657f)))

  test("KoyaSystem"):
    val points = computeTournamentPoints(allGames, playerA, KoyaSystem(LimitModifier.default))
    assertEquals(points, Some(TiebreakPoint(0.5f)))

  test("SumOfProgressiveScores"):
    val points = computeTournamentPoints(allGames, playerA, SumOfProgressiveScores(CutModifier.None))
    assertEquals(points, Some(TiebreakPoint(6.5f)))

  test("SumOfProgressiveScoresCut1"):
    val points = computeTournamentPoints(allGames, playerA, SumOfProgressiveScores(CutModifier.Cut1))
    assertEquals(points, Some(TiebreakPoint(5.5f)))

  test("SumOfProgressiveScoresMedian1"):
    val points =
      computeTournamentPoints(allGames, playerA, SumOfProgressiveScores(CutModifier.Median1))
    assertEquals(points, Some(TiebreakPoint(3f)))

  test("SumOfProgressiveScoresMedian2"):
    val points =
      computeTournamentPoints(allGames, playerA, SumOfProgressiveScores(CutModifier.Median2))
    assertEquals(points, Some(TiebreakPoint(0f)))

  test("PerfectTournamentPerformance - Perfect scores"):
    // from https://chess-results.com/tnr1166026.aspx?lan=1&art=1&rd=8
    val ruslan = Player("1", "Ruslan Pogorelov".some, rating = Elo(2255).some)
    val josep = Player("2", "Josep M. Beltran Reverter".some, rating = Elo(1834).some)
    val carles = Player("3", "Carles Costas Bella".some, rating = Elo(1929).some)
    val sergi = Player("4", "Sergi Aubanell Ber".some, rating = Elo(1988).some)
    val xavier = Player("5", "Xavier Palomo Teruel".some, rating = Elo(2145).some)
    val agusti = Player("6", "Agusti Guasch Figuerola".some, rating = Elo(1990).some)
    val daniel = Player("7", "Daniel Torrens Gonzalez".some, rating = Elo(1965).some)
    val aaron = Player("8", "Aaron Alfonso Pellisa".some, rating = Elo(2125).some)

    val ruslanGames = Seq(
      Game(Points.One, josep, Color.White, "1".some),
      Game(Points.One, carles, Color.Black, "2".some),
      Game(Points.One, sergi, Color.White, "3".some),
      Game(Points.Half, Player("0", "bye".some, None), Color.White, "4".some),
      Game(Points.One, xavier, Color.Black, "5".some),
      Game(Points.One, agusti, Color.White, "6".some),
      Game(Points.One, daniel, Color.Black, "7".some),
      Game(Points.One, aaron, Color.White, "8".some)
    )
    val ruslanPlayerGames = Seq(PlayerWithGames(ruslan, ruslanGames)).mapBy(_.player.id)
    assertEquals(
      computeTournamentPoints(ruslanPlayerGames, ruslan, TournamentPerformanceRating),
      Some(TiebreakPoint(2796f))
    )
    assertEquals(
      computeTournamentPoints(ruslanPlayerGames, ruslan, PerfectTournamentPerformance),
      Some(TiebreakPoint(2945f))
    ) // chess-results says 2949. Perfect scores though so :shrug:

  test("PerfectTournamentPerformance - Regular"):
    val marc = Player("1", "Marc Guardia Curto".some, rating = Elo(1830).some)
    val enric = Player("2", "Enric Regue Farran".some, rating = Elo(1914).some)
    val josepmg = Player("3", "Josep Maria Guasch Murtra".some, rating = Elo(1867).some)
    val francesc = Player("4", "Francesc Xavier Senso Moreno".some, rating = Elo(1818).some)
    val ruslan = Player("5", "Ruslan Pogorelov".some, rating = Elo(2255).some)
    val agusti = Player("6", "Agusti Guasch Figuerola".some, rating = Elo(1990).some)
    val xavier = Player("7", "Xavier Palomo Teruel".some, rating = Elo(2145).some)

    val xavierGames = Seq(
      Game(Points.One, marc, Color.White, "1".some),
      Game(Points.One, enric, Color.Black, "2".some),
      Game(Points.One, josepmg, Color.White, "3".some),
      Game(Points.Zero, ruslan, Color.White, "4".some),
      Game(Points.One, francesc, Color.Black, "5".some),
      // POVGame(Some(Points.Zero), Player("bye", None), Color.White), // Rd 7: bye (0)
      Game(Points.One, agusti, Color.White, "6".some)
    )
    // Lila excludes all bye games. So we don't need to check for them.
    val xavierPlayerGames = Seq(PlayerWithGames(xavier, xavierGames)).mapBy(_.player.id)
    assertEquals(
      computeTournamentPoints(xavierPlayerGames, xavier, TournamentPerformanceRating),
      Some(TiebreakPoint(2218f))
    )
    assertEquals(
      computeTournamentPoints(xavierPlayerGames, xavier, PerfectTournamentPerformance),
      Some(TiebreakPoint(2259f))
    )

  test("PerfectTournamentPerformance - Zero score"):
    val games = Seq(
      Game(Points.Zero, playerB, Color.Black, "1".some),
      Game(Points.Zero, playerC, Color.White, "2".some),
      Game(Points.Zero, playerD, Color.Black, "3".some)
    )
    val playerGames = Seq(PlayerWithGames(playerA, games)).mapBy(_.player.id)
    // Lowest rated opponent - 800
    assertEquals(
      computeTournamentPoints(playerGames, playerA, PerfectTournamentPerformance),
      Some(TiebreakPoint(650f))
    )

  test("AveragePerfectPerformanceOfOpponents"):
    val points = computeTournamentPoints(allGames, playerA, AveragePerfectPerformanceOfOpponents)
    assertEquals(
      points,
      Some(TiebreakPoint(1523f))
    ) // 1444 + 1549 + 1668 + 1432 = 6093 / 4 = 1523.25, rounded to 1523
