package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.tiebreak.Tiebreak.*
import chess.tiebreak.TiebreakPoint
import chess.tiebreak.*

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class TiebreakBench:

  private val Work: Long = 10

  var tournament: Tournament = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =
    val games = Helper.games("FWWRC.pgn")
    val lastRoundId = Helper.lastRoundId(games)
    tournament = Tournament(games, lastRoundId)

  @Benchmark
  def averageOfOpponentsBuchholz(bh: Blackhole) =
    bh.consume:
      AverageOfOpponentsBuchholz.compute(tournament, Map.empty)

  @Benchmark
  def averagePerfectPerformanceOfOpponents(bh: Blackhole) =
    bh.consume:
      AveragePerfectPerformanceOfOpponents.compute(tournament, Map.empty)

  @Benchmark
  def buchholz(bh: Blackhole) =
    bh.consume:
      Buchholz(CutModifier.None).compute(tournament, Map.empty)

  @Benchmark
  def directEncounter(bh: Blackhole) =
    bh.consume:
      DirectEncounter.compute(tournament, Map.empty)

  @Benchmark
  def perfectTournamentPerformance(bh: Blackhole) =
    bh.consume:
      PerfectTournamentPerformance.compute(tournament, Map.empty)

  @Benchmark
  def sonnebornBerger(bh: Blackhole) =
    bh.consume:
      SonnebornBerger(CutModifier.None).compute(tournament, Map.empty)

  @Benchmark
  def averageRatingOfOpponents(bh: Blackhole) =
    bh.consume:
      AverageRatingOfOpponents(CutModifier.None).compute(tournament, Map.empty)

  @Benchmark
  def foreBuchholz(bh: Blackhole) =
    bh.consume:
      ForeBuchholz(CutModifier.None).compute(tournament, Map.empty)

  @Benchmark
  def koyaSystem(bh: Blackhole) =
    bh.consume:
      KoyaSystem(LimitModifier.default).compute(tournament, Map.empty)

  @Benchmark
  def blackPlayedGames(bh: Blackhole) =
    bh.consume:
      NbBlackGames.compute(tournament, Map.empty)

  @Benchmark
  def blackWonGames(bh: Blackhole) =
    bh.consume:
      NbBlackWins.compute(tournament, Map.empty)

  @Benchmark
  def gamesWon(bh: Blackhole) =
    bh.consume:
      NbWins.compute(tournament, Map.empty)

  @Benchmark
  def tournamentPerformanceRating(bh: Blackhole) =
    bh.consume:
      TournamentPerformanceRating.compute(tournament, Map.empty)

  @Benchmark
  def averagePerformanceOfOpponents(bh: Blackhole) =
    bh.consume:
      AveragePerformanceOfOpponents.compute(tournament, Map.empty)

  @Benchmark
  def progressiveScores(bh: Blackhole) =
    bh.consume:
      SumOfProgressiveScores(CutModifier.None).compute(tournament, Map.empty)

  @Benchmark
  def fullTournament(bh: Blackhole) =
    bh.consume:
      tournament.compute(
        List(
          DirectEncounter,
          AverageOfOpponentsBuchholz,
          AveragePerfectPerformanceOfOpponents,
          PerfectTournamentPerformance,
          SonnebornBerger(CutModifier.None)
        )
      )
