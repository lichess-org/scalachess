package chess
package tiebreaker

import snapshot4s.generated.snapshotConfig
import snapshot4s.munit.SnapshotAssertions

import Helper.*

class TournamentTest extends MunitExtensions with SnapshotAssertions:

  test("tiebreaker games snapshot") {
    val result = Tiebreaker
      .compute(
        games("FWWRC.pgn"),
        List(
          AverageOfOpponentsBuchholz,
          AveragePerfectPerformanceOfOpponents,
          DirectEncounter,
          PerfectTournamentPerformance,
          SonnebornBerger
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreaker/tournament.txt")
  }

  // https://chess-results.com/tnr1074691.aspx?lan=1&art=1&flag=30
  test("Women's world rapid championship") {
    val result = Tiebreaker
      .compute(
        games("FWWRC.pgn"),
        List(
          Buchholz(Some(Modifier.Cut1)),
          Buchholz(),
          AverageRatingOfOpponentsCut1
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreaker/official_tournament.txt")
  }

  // https://chess-results.com/tnr1175851.aspx?art=1
  test("Uzchess Cup"):
    val result = Tiebreaker
      .compute(
        games("uzchesscup.pgn"),
        List(
          DirectEncounter,
          SonnebornBerger,
          NbWins,
          NbBlackWins,
          KoyaSystem
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreaker/uzchesscup.txt")
