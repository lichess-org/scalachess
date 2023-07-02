package benchmarks

import org.openjdk.jmh.annotations._

import chess.perft.Perft

import java.util.concurrent.TimeUnit
import chess.variant.*

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Threads(value = 1)
class PerftBench:

  var threecheckPerfts = Perft.threeCheckPerfts
  var nodeLimit = 10_000L
  var gameLimit = 100

  @Benchmark
  def threecheck() =
    bench(threecheckPerfts, ThreeCheck, nodeLimit, gameLimit)

  var antichessPerfts = Perft.antichessPerfts
  @Benchmark
  def antichess() =
    bench(antichessPerfts, Antichess, nodeLimit, gameLimit)

  var atomicPerfts = Perft.atomicPerfts
  @Benchmark
  def atomic() =
    bench(atomicPerfts, Atomic, nodeLimit, gameLimit)

  var crazyhousePerfts = Perft.crazyhousePerfts
  @Benchmark
  def crazyhouse() =
    bench(crazyhousePerfts, Crazyhouse, nodeLimit, gameLimit)

  var hordePerfts = Perft.hordePerfts
  @Benchmark
  def horde() =
    bench(hordePerfts, Horde, nodeLimit, gameLimit)

  var racingkingsPerfts = Perft.racingkingsPerfts
  @Benchmark
  def racingkings() =
    bench(racingkingsPerfts, RacingKings, nodeLimit, gameLimit)

  var randomPerfts = Perft.randomPerfts.take(50)
  @Benchmark
  def chess960() =
    bench(randomPerfts, Chess960, nodeLimit, gameLimit)

  var trickyPerfts = Perft.trickyPerfts
  @Benchmark
  def tricky() =
    bench(trickyPerfts, Chess960, nodeLimit, gameLimit)

  private def bench(perfts: List[Perft], variant: Variant, nodeLimit: Long, gameLimit: Int) =
    perfts.take(gameLimit).map(_.withLimit(nodeLimit).calculate(variant))
