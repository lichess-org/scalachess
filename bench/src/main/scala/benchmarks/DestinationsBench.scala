package benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.infra.Blackhole
import chess.Situation
import chess.format.*
import chess.perft.Perft
import chess.variant.*

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(3)
@Threads(value = 1)
class DestinationsBench:

  private val Work: Long = 5

  @Param(Array("100"))
  var games: Int = scala.compiletime.uninitialized

  var threecheckInput: List[Situation]  = scala.compiletime.uninitialized
  var antichessInput: List[Situation]   = scala.compiletime.uninitialized
  var atomicInput: List[Situation]      = scala.compiletime.uninitialized
  var crazyhouseInput: List[Situation]  = scala.compiletime.uninitialized
  var racingkingsInput: List[Situation] = scala.compiletime.uninitialized
  var hordeInput: List[Situation]       = scala.compiletime.uninitialized
  var randomInput: List[Situation]      = scala.compiletime.uninitialized
  var trickyInput: List[Situation]      = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =
    threecheckInput = makeSituations(Perft.threeCheckPerfts, ThreeCheck, games)
    antichessInput = makeSituations(Perft.antichessPerfts, Antichess, games)
    atomicInput = makeSituations(Perft.atomicPerfts, Atomic, games)
    crazyhouseInput = makeSituations(Perft.crazyhousePerfts, Crazyhouse, games)
    racingkingsInput = makeSituations(Perft.racingkingsPerfts, RacingKings, games)
    hordeInput = makeSituations(Perft.hordePerfts, Horde, games)
    randomInput = makeSituations(Perft.randomPerfts, Chess960, games)
    trickyInput = makeSituations(Perft.trickyPerfts, Chess960, games)

  @Benchmark
  def threecheck(bh: Blackhole) =
    bench(threecheckInput)(bh)

  @Benchmark
  def antichess(bh: Blackhole) =
    bench(antichessInput)(bh)

  @Benchmark
  def atomic(bh: Blackhole) =
    bench(atomicInput)(bh)

  @Benchmark
  def crazyhouse(bh: Blackhole) =
    bench(crazyhouseInput)(bh)

  @Benchmark
  def horde(bh: Blackhole) =
    bench(hordeInput)(bh)

  @Benchmark
  def racingkings(bh: Blackhole) =
    bench(racingkingsInput)(bh)

  @Benchmark
  def chess960(bh: Blackhole) =
    bench(randomInput)(bh)

  @Benchmark
  def tricky(bh: Blackhole) =
    bench(trickyInput)(bh)

  private def bench(sits: List[Situation])(bh: Blackhole) =
    val x = sits.map: x =>
      Blackhole.consumeCPU(Work)
      x.destinations
    bh.consume(x)

  private def makeSituations(perfts: List[Perft], variant: Variant, games: Int): List[Situation] =
    perfts
      .take(games)
      .map(p => Fen.read(variant, p.epd).getOrElse(throw RuntimeException("boooo")))
