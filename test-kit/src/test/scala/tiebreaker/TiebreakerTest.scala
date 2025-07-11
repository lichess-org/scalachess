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
  val allGames      =
    Seq(playerA_Games, playerB_Games, playerC_Games, playerD_Games, playerE_Games).mapBy(_.player.id)

  def computeTournamentPoints(
      allGames: Map[PlayerId, PlayerWithGames],
      player: Player,
      tiebreak: Tiebreaker
  ): Option[TieBreakPoints] =
    Tournament(allGames)
      .compute(List(tiebreak))
      .find(_.player.id == player.id)
      .flatMap(_.tiebreakers.headOption)

  test("scores"):
    assertEquals(playerA_Games.games.score, 2.5f)
    assertEquals(playerB_Games.games.score, 1.5f)
    assertEquals(playerC_Games.games.score, 2.0f)
    assertEquals(playerD_Games.games.score, 2.5f)
    assertEquals(playerE_Games.games.score, 1.5f)

  test("NbBlackGames"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, NbBlackGames)
    assertEquals(tiebreaker, Some(TieBreakPoints(1.0f)))

  test("NbWins"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, NbWins)
    assertEquals(tiebreaker, Some(TieBreakPoints(2.0f)))

  test("NbBlackWins"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, NbBlackWins)
    assertEquals(tiebreaker, Some(TieBreakPoints(0f)))

  test("SonnebornBerger"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, SonnebornBerger)
    assertEquals(tiebreaker, Some(TieBreakPoints(4.0f)))

  test("SonnebornBergerCut1"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, SonnebornBergerCut1)
    assertEquals(tiebreaker, Some(TieBreakPoints(4.0f)))

  test("Buchholz"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, Buchholz())
    assertEquals(tiebreaker, Some(TieBreakPoints(7.5f)))

  test("BuchholzCut1"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, Buchholz(Some(Modifier.Cut1)))
    assertEquals(tiebreaker, Some(TieBreakPoints(6f)))

  test("BuchholzCut2"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, Buchholz(Some(Modifier.Cut2)))
    assertEquals(tiebreaker, Some(TieBreakPoints(4.5f)))

  test("BuchholzMedian1"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, Buchholz(Some(Modifier.Median1)))
    assertEquals(tiebreaker, Some(TieBreakPoints(3.5f)))

  test("BuchholzMedian2"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, Buchholz(Some(Modifier.Median2)))
    assertEquals(tiebreaker, Some(TieBreakPoints(0f)))

  test("ForeBuchholz"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, ForeBuchholz)
    assertEquals(tiebreaker, Some(TieBreakPoints(8f)))

  test("ForeBuchholzCut1"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, ForeBuchholzCut1)
    assertEquals(tiebreaker, Some(TieBreakPoints(6f)))

  test("AverageOfOpponentsBuchholz"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, AverageOfOpponentsBuchholz)
    assertEquals(tiebreaker, Some(TieBreakPoints(8.125f)))

  test("DirectEncounter"):
    val tiebreaker1 = computeTournamentPoints(allGames, playerA, DirectEncounter)
    val tiebreaker2 = computeTournamentPoints(allGames, playerD, DirectEncounter)
    assertEquals(tiebreaker1, Some(TieBreakPoints(0f)))
    assertEquals(tiebreaker2, Some(TieBreakPoints(1f)))

  test("DirectEncounter with more than one game"):
    val extraDraw = Seq(
      playerA_Games.copy(games = playerA_Games.games ++ Seq(Game(Some(Points.Half), playerD, Color.White))),
      playerB_Games,
      playerC_Games,
      playerD_Games.copy(games = playerD_Games.games ++ Seq(Game(Some(Points.Half), playerA, Color.Black))),
      playerE_Games
    ).mapBy(_.player.id)
    val tiebreaker1 = computeTournamentPoints(extraDraw, playerD, DirectEncounter)
    val tiebreaker2 = computeTournamentPoints(extraDraw, playerA, DirectEncounter)
    assertEquals(tiebreaker1, Some(TieBreakPoints(0.75f)))
    assertEquals(tiebreaker2, Some(TieBreakPoints(0.25f)))

  test("DirectEncounter with unequal partial tiebreaks"):
    val previousPoints = Map(
      playerA.id -> List(TieBreakPoints(1f)),
      playerD.id -> List(TieBreakPoints(0.5f))
    )

    val tiebreaker1 = DirectEncounter
      .compute(Tournament(allGames), previousPoints)
      .get(playerA.id)
      .flatMap(_.lift(1))
    val tiebreaker2 = DirectEncounter
      .compute(Tournament(allGames), previousPoints)
      .get(playerD.id)
      .flatMap(_.lift(1))
    assertEquals(tiebreaker1, Some(TieBreakPoints(0f)))
    assertEquals(tiebreaker2, Some(TieBreakPoints(0f)))

  test("DirectEncounter with equal partial tiebreaks"):

    val previousPoints = Map(
      playerA.id -> List(TieBreakPoints(1f)),
      playerD.id -> List(TieBreakPoints(1f))
    )

    val tiebreaker1 = DirectEncounter
      .compute(Tournament(allGames), previousPoints)
      .get(playerA.id)
      .flatMap(_.lift(1))
    val tiebreaker2 = DirectEncounter
      .compute(Tournament(allGames), previousPoints)
      .get(playerD.id)
      .flatMap(_.lift(1))
    assertEquals(tiebreaker1, Some(TieBreakPoints(0f)))
    assertEquals(tiebreaker2, Some(TieBreakPoints(1f)))

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
      playerA.id              -> List(TieBreakPoints(1f)),
      playerD.id              -> List(TieBreakPoints(1f)),
      playerX_Games.player.id -> List(TieBreakPoints(1f))
    )

    val allGamesWithPartial =
      Seq(playerA_Games, playerD_Games, playerC_Games, playerD_Games, playerE_Games, playerX_Games)
        .mapBy(_.player.id)

    val tiebreaker1 = DirectEncounter
      .compute(Tournament(allGamesWithPartial), previousPoints)
      .get(playerA.id)
      .flatMap(_.lift(1))
    val tiebreaker2 = DirectEncounter
      .compute(Tournament(allGamesWithPartial), previousPoints)
      .get(playerD.id)
      .flatMap(_.lift(1))
    val tiebreakerX = DirectEncounter
      .compute(Tournament(allGamesWithPartial), previousPoints)
      .get(playerX_Games.player.id)
      .flatMap(_.lift(1))
    assertEquals(tiebreaker1, Some(TieBreakPoints(0f)))
    assertEquals(tiebreaker2, Some(TieBreakPoints(0f)))
    assertEquals(tiebreakerX, Some(TieBreakPoints(0f)))

  test("AverageOpponentRating"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, AverageRatingOfOpponents)
    assertEquals(tiebreaker, Some(TieBreakPoints(1563f)))

  test("AverageOpponentRating with unrated opponents"):
    val unratedOpponent = Player("Unrated Opponent", rating = None)
    val unratedGames    = Seq(
      Game(Some(Points.One), unratedOpponent, Color.White)
    )
    val unratedPlayerGames = Seq(PlayerWithGames(playerA, unratedGames))
    val tiebreaker         =
      computeTournamentPoints(unratedPlayerGames.mapBy(_.player.id), playerA, AverageRatingOfOpponents)
    assertEquals(tiebreaker, Some(TieBreakPoints(0f)))
    assertEquals(
      computeTournamentPoints(
        allGames.updated(playerA.id, playerA_Games.copy(games = playerA_Games.games ++ unratedGames)),
        playerA,
        AverageRatingOfOpponents
      ),
      Some(TieBreakPoints(1563f))
    )

  test("AveragePerformanceOfOpponents"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, AveragePerformanceOfOpponents)
    assertEquals(tiebreaker, Some(TieBreakPoints(1527f)))

  test("TournamentPerformanceRating"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, TournamentPerformanceRating)
    assertEquals(tiebreaker, Some(TieBreakPoints(1657f)))

  test("KoyaSystem"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, KoyaSystem)
    assertEquals(tiebreaker, Some(TieBreakPoints(0.5f)))

  test("SumOfProgressiveScores"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, SumOfProgressiveScores())
    assertEquals(tiebreaker, Some(TieBreakPoints(6.5f)))

  test("SumOfProgressiveScoresCut1"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, SumOfProgressiveScores(Some(Modifier.Cut1)))
    assertEquals(tiebreaker, Some(TieBreakPoints(5.5f)))

  test("SumOfProgressiveScoresMedian1"):
    val tiebreaker =
      computeTournamentPoints(allGames, playerA, SumOfProgressiveScores(Some(Modifier.Median1)))
    assertEquals(tiebreaker, Some(TieBreakPoints(3f)))

  test("SumOfProgressiveScoresMedian2"):
    val tiebreaker =
      computeTournamentPoints(allGames, playerA, SumOfProgressiveScores(Some(Modifier.Median2)))
    assertEquals(tiebreaker, Some(TieBreakPoints(0f)))

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
    assertEquals(
      computeTournamentPoints(ruslanPlayerGames, ruslan, TournamentPerformanceRating),
      Some(TieBreakPoints(2796f))
    )
    assertEquals(
      computeTournamentPoints(ruslanPlayerGames, ruslan, PerfectTournamentPerformance),
      Some(TieBreakPoints(2945f))
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
    assertEquals(
      computeTournamentPoints(xavierPlayerGames, xavier, TournamentPerformanceRating),
      Some(TieBreakPoints(2218f))
    )
    assertEquals(
      computeTournamentPoints(xavierPlayerGames, xavier, PerfectTournamentPerformance),
      Some(TieBreakPoints(2259f))
    )

  test("PerfectTournamentPerformance - Zero score"):
    val games = Seq(
      Game(Some(Points.Zero), playerB, Color.Black),
      Game(Some(Points.Zero), playerC, Color.White),
      Game(Some(Points.Zero), playerD, Color.Black)
    )
    val playerGames = Seq(PlayerWithGames(playerA, games)).mapBy(_.player.id)
    // Lowest rated opponent - 800
    assertEquals(
      computeTournamentPoints(playerGames, playerA, PerfectTournamentPerformance),
      Some(TieBreakPoints(650f))
    )

  test("AveragePerfectPerformanceOfOpponents"):
    val tiebreaker = computeTournamentPoints(allGames, playerA, AveragePerfectPerformanceOfOpponents)
    assertEquals(
      tiebreaker,
      Some(TieBreakPoints(1523f))
    ) // 1444 + 1549 + 1668 + 1432 = 6093 / 4 = 1523.25, rounded to 1523
