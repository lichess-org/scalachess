package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import chess.perft.Perft
import chess.variant.*

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class PerftBench:

  // the unit of CPU work per iteration
  private val Work: Long = 10

  @Param(Array("10"))
  var games: Int = scala.compiletime.uninitialized

  @Param(Array("10000", "100000", "1000000", "10000000"))
  var nodes: Long = scala.compiletime.uninitialized

  var threecheckPerfts: List[Perft]  = scala.compiletime.uninitialized
  var antichessPerfts: List[Perft]   = scala.compiletime.uninitialized
  var atomicPerfts: List[Perft]      = scala.compiletime.uninitialized
  var crazyhousePerfts: List[Perft]  = scala.compiletime.uninitialized
  var racingkingsPerfts: List[Perft] = scala.compiletime.uninitialized
  var hordePerfts: List[Perft]       = scala.compiletime.uninitialized
  var randomPerfts: List[Perft]      = scala.compiletime.uninitialized
  var trickyPerfts: List[Perft]      = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =
    threecheckPerfts = makePerft(Perft.threeCheckPerfts, games, nodes)
    antichessPerfts = makePerft(Perft.antichessPerfts, games, nodes)
    atomicPerfts = makePerft(Perft.atomicPerfts, games, nodes)
    crazyhousePerfts = makePerft(Perft.crazyhousePerfts, games, nodes)
    racingkingsPerfts = makePerft(Perft.racingkingsPerfts, games, nodes)
    hordePerfts = makePerft(Perft.hordePerfts, games, nodes)
    randomPerfts = makePerft(Perft.randomPerfts, games, nodes)
    trickyPerfts = makePerft(Perft.trickyPerfts, games, nodes)

  @Benchmark
  def threecheck(bh: Blackhole) =
    bench(threecheckPerfts, ThreeCheck)(bh)

  @Benchmark
  def antichess(bh: Blackhole) =
    bench(antichessPerfts, Antichess)(bh)

  @Benchmark
  def atomic(bh: Blackhole) =
    bench(atomicPerfts, Atomic)(bh)

  @Benchmark
  def crazyhouse(bh: Blackhole) =
    bench(crazyhousePerfts, Crazyhouse)(bh)

  @Benchmark
  def horde(bh: Blackhole) =
    bench(hordePerfts, Horde)(bh)

  @Benchmark
  def racingkings(bh: Blackhole) =
    bench(racingkingsPerfts, RacingKings)(bh)

  @Benchmark
  def chess960(bh: Blackhole) =
    bench(randomPerfts, Chess960)(bh)

  @Benchmark
  def tricky(bh: Blackhole) =
    bench(trickyPerfts, Chess960)(bh)

  private def makePerft(perfts: List[Perft], games: Int, nodes: Long) =
    perfts.take(games).map(_.withLimit(nodes))

  private def bench(perfts: List[Perft], variant: Variant)(bh: Blackhole) =
    var i = 0
    while i < perfts.size do
      val game = perfts(i)
      Blackhole.consumeCPU(Work)
      bh.consume(game.calculate(variant))
      i += 1
