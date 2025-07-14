package chess
package tiebreak

import snapshot4s.generated.snapshotConfig
import snapshot4s.munit.SnapshotAssertions

import Helper.*

class TiebreakSnapshotTest extends MunitExtensions with SnapshotAssertions:

  test("tiebreak games snapshot") {
    val result = Tiebreak
      .compute(
        games("FWWRC.pgn"),
        List(
          AverageOfOpponentsBuchholz,
          AveragePerfectPerformanceOfOpponents,
          DirectEncounter,
          PerfectTournamentPerformance,
          SonnebornBerger(CutModifier.None)
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreak/tournament.txt")
  }

  // https://chess-results.com/tnr1074691.aspx?lan=1&art=1&flag=30
  test("Women's world rapid championship") {
    val result = Tiebreak
      .compute(
        games("FWWRC.pgn"),
        List(
          Buchholz(CutModifier.Cut1),
          Buchholz(CutModifier.None),
          AverageRatingOfOpponents(CutModifier.Cut1)
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreak/official_tournament.txt")
  }

  // https://chess-results.com/tnr1175851.aspx?art=1
  test("Uzchess Cup"):
    val result = Tiebreak
      .compute(
        games("uzchesscup.pgn"),
        List(
          DirectEncounter,
          SonnebornBerger(CutModifier.None),
          NbWins,
          NbBlackWins,
          KoyaSystem(LimitModifier(0.5f))
        )
      )
      .mkString("\n")
    assertFileSnapshot(result, "tiebreak/uzchesscup.txt")
