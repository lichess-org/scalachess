package chess

import chess.tiebreaker.Tiebreaker.*
import chess.rating.Elo
import chess.Outcome.Points
import cats.data.NonEmptySeq
import chess.tiebreaker.TieBreakPoints
import chess.tiebreaker.score
import cats.syntax.all.*

class TiebreakersTest extends ChessTest:

  val playerA = Player("PlayerA", rating = Elo(1500).some)
  val playerB = Player("PlayerB", rating = Elo(1600).some)
  val playerC = Player("PlayerC", rating = Elo(1550).some)
  val playerD = Player("PlayerD", rating = Elo(1450).some)
  val playerE = Player("PlayerE", rating = Elo(1650).some)

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

  test("AverageOfOpponentsBuchholz"):
    val tiebreaker = tb(AverageOfOpponentsBuchholz, playerA, allGames)
    assertEquals(tiebreaker, 8.125f)

  test("DirectEncounter"):
    val tiebreaker1 = tb(DirectEncounter, playerA, allGames)
    val tiebreaker2 = tb(DirectEncounter, playerD, allGames)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 1f)

  test("DirectEncounter with unequal partial tiebreaks"):
    val playerAWithPartial =
      playerA_Games.copy(partialTiebreaks = NonEmptySeq.fromSeq(Seq(TieBreakPoints(1f))))
    val playerDWithPartial =
      playerD_Games.copy(partialTiebreaks = NonEmptySeq.fromSeq(Seq(TieBreakPoints(0.5f))))
    val allGamesWithPartial =
      Seq(playerAWithPartial, playerB_Games, playerC_Games, playerDWithPartial, playerE_Games)

    val tiebreaker1 = tb(DirectEncounter, playerAWithPartial.player, allGamesWithPartial)
    val tiebreaker2 = tb(DirectEncounter, playerDWithPartial.player, allGamesWithPartial)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 0f)

  test("DirectEncounter with equal partial tiebreaks"):
    val playerAWithPartial =
      playerA_Games.copy(partialTiebreaks = NonEmptySeq.fromSeq(Seq(TieBreakPoints(1f))))
    val playerDWithPartial =
      playerD_Games.copy(partialTiebreaks = NonEmptySeq.fromSeq(Seq(TieBreakPoints(1f))))
    val allGamesWithPartial =
      Seq(playerAWithPartial, playerB_Games, playerC_Games, playerDWithPartial, playerE_Games)

    val tiebreaker1 = tb(DirectEncounter, playerAWithPartial.player, allGamesWithPartial)
    val tiebreaker2 = tb(DirectEncounter, playerDWithPartial.player, allGamesWithPartial)
    assertEquals(tiebreaker1, 0f)
    assertEquals(tiebreaker2, 1f)

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
