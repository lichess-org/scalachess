package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.tiebreaker.Tiebreaker.*
import chess.tiebreaker.TieBreakPoints
import chess.tiebreaker.*

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class TiebreakerBench:

  private val Work: Long = 10

  var allGames: Map[PlayerId, PlayerWithGames] = scala.compiletime.uninitialized
  var tournament: Tournament                   = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =
    allGames = Helper.games("FWWRC.pgn")
    tournament = Tournament(allGames)

  @Benchmark
  def averageOfOpponentsBuchholz(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        AverageOfOpponentsBuchholz.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def averageOfOpponentsBuchholzAll(bh: Blackhole) =
    bh.consume:
      AverageOfOpponentsBuchholz.compute(tournament, Map.empty)

  @Benchmark
  def averagePerfectPerformanceOfOpponents(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        AveragePerfectPerformanceOfOpponents.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def averagePerfectPerformanceOfOpponentsAll(bh: Blackhole) =
    bh.consume:
      AveragePerfectPerformanceOfOpponents.compute(tournament, Map.empty)

  @Benchmark
  def directEncounter(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        DirectEncounter.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def directEncounterAll(bh: Blackhole) =
    bh.consume:
      DirectEncounter.compute(tournament, Map.empty)

  @Benchmark
  def perfectTournamentPerformance(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        PerfectTournamentPerformance.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def perfectTournamentPerformanceAll(bh: Blackhole) =
    bh.consume:
      PerfectTournamentPerformance.compute(tournament, Map.empty)

  @Benchmark
  def sonnebornBerger(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        SonnebornBerger.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def sonnebornBergerAll(bh: Blackhole) =
    bh.consume:
      SonnebornBerger.compute(tournament, Map.empty)

  @Benchmark
  def tournamentPerformance(bh: Blackhole) =
    bh.consume:
      Tiebreaker.compute(
        allGames,
        List(
          AverageOfOpponentsBuchholz,
          AveragePerfectPerformanceOfOpponents,
          DirectEncounter,
          PerfectTournamentPerformance,
          SonnebornBerger
        )
      )

  @Benchmark
  def averageRatingOfOpponents(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        AverageRatingOfOpponents.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def averageRatingOfOpponentsAll(bh: Blackhole) =
    bh.consume:
      AverageRatingOfOpponents.compute(tournament, Map.empty)

  @Benchmark
  def foreBuchholz(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        ForeBuchholz.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def koyaSystem(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        KoyaSystem.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def blackPlayedGames(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        NbBlackGames.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def blackWonGames(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        NbBlackWins.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def gamesWon(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        NbWins.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def tournamentPerformanceRating(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        TournamentPerformanceRating.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def averagePerformanceOfOpponents(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        AveragePerformanceOfOpponents.compute(pg.player, allGames, Map.empty)

  @Benchmark
  def progressiveScores(bh: Blackhole) =
    bh.consume:
      allGames.values.map: pg =>
        Blackhole.consumeCPU(Work)
        SumOfProgressiveScores.compute(pg.player, allGames, Map.empty)
