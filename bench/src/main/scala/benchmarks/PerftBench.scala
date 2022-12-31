package benchmarks

import org.openjdk.jmh.annotations._

import chess.perft.Perft

import java.util.concurrent.TimeUnit
import chess.variant.*

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 10, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 3, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(2)
class PerftBench {

  var threecheckPerfts = Perft.threeCheckPerfts
  @Benchmark
  def thresscheck() =
    bench(threecheckPerfts, ThreeCheck, 100_000L)

  var antichessPerfts = Perft.antichessPerfts
  @Benchmark
  def antichess() =
    bench(antichessPerfts, Antichess, 100_000L)

  var atomicPerfts = Perft.atomicPerfts
  @Benchmark
  def atomic() =
    bench(atomicPerfts, Atomic, 100_000L)

  var crazyhousePerfts = Perft.crazyhousePerfts
  @Benchmark
  def cracyhouse() =
    bench(crazyhousePerfts, Crazyhouse, 100_000L)

  var hordePerfts = Perft.hordePerfts
  @Benchmark
  def horde() =
    bench(hordePerfts, Horde, 100_000L)

  var racingkingsPerfts = Perft.racingkingsPerfts
  @Benchmark
  def racingkings() =
    bench(racingkingsPerfts, RacingKings, 100_000L)

  var randomPerfts = Perft.randomPerfts.take(50)
  @Benchmark
  def chess960() =
    bench(randomPerfts, Chess960, 10_000L)

  var trickyPerfts = Perft.trickyPerfts
  @Benchmark
  def tricky() =
    bench(trickyPerfts, Chess960, 100_000L)

  private def bench(perfts: List[Perft], variant: Variant, nodeLimit: Long) =
    perfts.map(_.withLimit(nodeLimit).calculate(variant))

}
