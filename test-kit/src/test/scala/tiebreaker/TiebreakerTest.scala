package chess

import cats.syntax.all.*
import chess.Outcome.Points
import chess.rating.Elo
import chess.tiebreaker.Tiebreaker.*
import chess.tiebreaker.{ TieBreakPoints, score }

class TiebreakersTest extends ChessTest:

  val playerA = Player("PlayerA", rating = Elo(1500).some)
  val playerB = Player("PlayerB", rating = Elo(1600).some)
  val playerC = Player("PlayerC", rating = Elo(1550).some)
  val playerD = Player("PlayerD", rating = Elo(1450).some)
  val playerE = Player("PlayerE", rating = Elo(1650).some)

  case class Game(white: Player, black: Player, result: ByColor[Points])

  extension (p: Player)
    def beats(opponent: Player) = Game(p, opponent, ByColor(Points.One, Points.Zero))
    def draws(opponent: Player) = Game(p, opponent, ByColor(Points.Half, Points.Half))
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

  def povGames(player: Player): Seq[POVGame] =
    games.collect:
      case Game(white, black, result) if white == player || black == player =>
        val playerColor = Color.fromWhite(white == player)
        POVGame(Some(result(playerColor)), playerColor.fold(black, white), playerColor)

  val playerA_Games     = PlayerGames(playerA, povGames(playerA))
  val playerB_Games     = PlayerGames(playerB, povGames(playerB))
  val playerC_Games     = PlayerGames(playerC, povGames(playerC))
  val playerD_Games     = PlayerGames(playerD, povGames(playerD))
  val playerE_Games     = PlayerGames(playerE, povGames(playerE))
  val allGames          = Seq(playerA_Games, playerB_Games, playerC_Games, playerD_Games, playerE_Games)
  val playerA_opponents = Seq(playerB_Games, playerC_Games, playerD_Games, playerE_Games)

  test("scores"):
    assertEquals(playerA_Games.games.score, 2.5f)
    assertEquals(playerB_Games.games.score, 1.5f)
    assertEquals(playerC_Games.games.score, 2.0f)
    assertEquals(playerD_Games.games.score, 2.5f)
    assertEquals(playerE_Games.games.score, 1.5f)

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

  test("ForeBuchholz"):
    val tiebreaker = tb(ForeBuchholz, playerA, allGames)
    assertEquals(tiebreaker, 8f)

  test("ForeBuchholzCut1"):
    val tiebreaker = tb(ForeBuchholzCut1, playerA, allGames)
    assertEquals(tiebreaker, 6f)

  test("AverageOfOpponentsBuchholz"):
    val tiebreaker = tb(AverageOfOpponentsBuchholz, playerA, allGames)
    assertEquals(tiebreaker, 8.125f)

  test("DirectEncounter"):
    val tiebreaker1 = tb(DirectEncounter, playerA, allGames)
    val tiebreaker2 = tb(DirectEncounter, playerD, allGames)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 1f)

  test("DirectEncounter with more than one game"):
    val extraDraw = Seq(
      playerA_Games.copy(games =
        playerA_Games.games ++ Seq(POVGame(Some(Points.Half), playerD, Color.White))
      ),
      playerB_Games,
      playerC_Games,
      playerD_Games.copy(games =
        playerD_Games.games ++ Seq(POVGame(Some(Points.Half), playerA, Color.Black))
      ),
      playerE_Games
    )
    val tiebreaker1 = tb(DirectEncounter, playerD, extraDraw)
    val tiebreaker2 = tb(DirectEncounter, playerA, extraDraw)
    assertEquals(tiebreaker1, 0.75f) // 1 win + 1 draw / 2 games
    assertEquals(tiebreaker2, 0.25f) // 1 draw / 2 games

  test("DirectEncounter with unequal partial tiebreaks"):
    val playerAWithPartial  = playerA_Games.copy(partialTiebreaks = List(TieBreakPoints(1f)))
    val playerDWithPartial  = playerD_Games.copy(partialTiebreaks = List(TieBreakPoints(0.5f)))
    val allGamesWithPartial =
      Seq(playerAWithPartial, playerB_Games, playerC_Games, playerDWithPartial, playerE_Games)

    val tiebreaker1 = tb(DirectEncounter, playerAWithPartial.player, allGamesWithPartial)
    val tiebreaker2 = tb(DirectEncounter, playerDWithPartial.player, allGamesWithPartial)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 0f)

  test("DirectEncounter with equal partial tiebreaks"):
    val playerAWithPartial  = playerA_Games.copy(partialTiebreaks = List(TieBreakPoints(1f)))
    val playerDWithPartial  = playerD_Games.copy(partialTiebreaks = List(TieBreakPoints(1f)))
    val allGamesWithPartial =
      Seq(playerAWithPartial, playerB_Games, playerC_Games, playerDWithPartial, playerE_Games)

    val tiebreaker1 = tb(DirectEncounter, playerAWithPartial.player, allGamesWithPartial)
    val tiebreaker2 = tb(DirectEncounter, playerDWithPartial.player, allGamesWithPartial)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 1f)

  test("DirectEncounter with equal partial tiebreaks but not all players have met"):
    val playerAWithPartial = playerA_Games.copy(partialTiebreaks = List(TieBreakPoints(1f)))
    val playerDWithPartial = playerD_Games.copy(partialTiebreaks = List(TieBreakPoints(1f)))
    // Create a player X that has not played against A or D
    // A, D and X are all on 2.5 points with partial tiebreaks of 1 but only A and D have met
    val playerX_Games = PlayerGames(
      Player("PlayerX", rating = Elo(1500).some),
      Seq(
        POVGame(Some(Points.Half), playerB, Color.White),
        POVGame(Some(Points.One), playerD, Color.White),
        POVGame(Some(Points.One), playerE, Color.Black)
      ),
      List(TieBreakPoints(1f))
    )
    val allGamesWithPartial =
      Seq(playerAWithPartial, playerDWithPartial, playerC_Games, playerD_Games, playerE_Games, playerX_Games)

    val tiebreaker1 = tb(DirectEncounter, playerAWithPartial.player, allGamesWithPartial)
    val tiebreaker2 = tb(DirectEncounter, playerDWithPartial.player, allGamesWithPartial)
    val tiebreakerX = tb(DirectEncounter, playerX_Games.player, allGamesWithPartial)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 0f)
    assertEquals(tiebreakerX, 0f)

  test("AverageOpponentRating"):
    val tiebreaker = tb(AverageRatingOfOpponents, playerA, allGames)
    assertEquals(tiebreaker, 1563f)

  test("AverageOpponentRating with unrated opponents"):
    val unratedOpponent = Player("Unrated Opponent", rating = None)
    val unratedGames    = Seq(
      POVGame(Some(Points.One), unratedOpponent, Color.White)
    )
    val unratedPlayerGames = PlayerGames(unratedOpponent, unratedGames)
    val tiebreaker         = tb(AverageRatingOfOpponents, playerA, Seq(unratedPlayerGames))
    assertEquals(tiebreaker, 0f)
    assertEquals(tb(AverageRatingOfOpponents, playerA, allGames :+ unratedPlayerGames), 1563f)

  test("AveragePerformanceOfOpponents"):
    val tiebreaker = tb(AveragePerformanceOfOpponents, playerA, allGames)
    assertEquals(tiebreaker, 1527f)

  test("TournamentPerformanceRating"):
    val tiebreaker = tb(TournamentPerformanceRating, playerA, allGames)
    assertEquals(tiebreaker, 1657f)

  test("KoyaSystem"):
    val tiebreaker = tb(KoyaSystem, playerA, allGames)
    assertEquals(tiebreaker, 0.5f)

  test("SumOfProgressiveScores"):
    val tiebreaker = tb(SumOfProgressiveScores, playerA, allGames)
    assertEquals(tiebreaker, 6.5f)

  test("SumOfProgressiveScoresCut1"):
    val tiebreaker = tb(SumOfProgressiveScoresCut1, playerA, allGames)
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
      POVGame(Some(Points.One), josep, Color.White),
      POVGame(Some(Points.One), carles, Color.Black),
      POVGame(Some(Points.One), sergi, Color.White),
      POVGame(Some(Points.Half), Player("bye", None), Color.White),
      POVGame(Some(Points.One), xavier, Color.Black),
      POVGame(Some(Points.One), agusti, Color.White),
      POVGame(Some(Points.One), daniel, Color.Black),
      POVGame(Some(Points.One), aaron, Color.White)
    )
    val ruslanPlayerGames = PlayerGames(ruslan, ruslanGames)
    assertEquals(tb(TournamentPerformanceRating, ruslan, Seq(ruslanPlayerGames)), 2796f)
    assertEquals(
      tb(PerfectTournamentPerformance, ruslan, Seq(ruslanPlayerGames)),
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
      POVGame(Some(Points.One), marc, Color.White),
      POVGame(Some(Points.One), enric, Color.Black),
      POVGame(Some(Points.One), josepmg, Color.White),
      POVGame(Some(Points.Zero), ruslan, Color.White),
      POVGame(Some(Points.One), francesc, Color.Black),
      // POVGame(Some(Points.Zero), Player("bye", None), Color.White), // Rd 7: bye (0)
      POVGame(Some(Points.One), agusti, Color.White)
    )
    // Lila excludes all bye games. So we don't need to check for them.
    val xavierPlayerGames = PlayerGames(xavier, xavierGames)
    assertEquals(tb(TournamentPerformanceRating, xavier, Seq(xavierPlayerGames)), 2218f)
    assertEquals(tb(PerfectTournamentPerformance, xavier, Seq(xavierPlayerGames)), 2259f)

  test("PerfectTournamentPerformance - Zero score"):
    val games = Seq(
      POVGame(Some(Points.Zero), playerB, Color.Black),
      POVGame(Some(Points.Zero), playerC, Color.White),
      POVGame(Some(Points.Zero), playerD, Color.Black)
    )
    val playerGames = PlayerGames(playerA, games)
    // Lowest rated opponent - 800
    assertEquals(tb(PerfectTournamentPerformance, playerA, Seq(playerGames)), 650f)

  test("AveragePerfectPerformanceOfOpponents"):
    val tiebreaker = tb(AveragePerfectPerformanceOfOpponents, playerA, allGames)
    assertEquals(tiebreaker, 1523f) // 1444 + 1549 + 1668 + 1432 = 6093 / 4 = 1523.25, rounded to 1523
