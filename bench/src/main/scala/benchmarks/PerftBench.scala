package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import chess.perft.{ Perft, Result }
import chess.format.Fen
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
  private[this] val Work: Long = 10

  @Param(Array("50"))
  var games: Int = _

  @Param(Array("10000"))
  var nodes: Long = _

  var threecheckPerfts: List[Perft]  = _
  var antichessPerfts: List[Perft]   = _
  var atomicPerfts: List[Perft]      = _
  var crazyhousePerfts: List[Perft]  = _
  var racingkingsPerfts: List[Perft] = _
  var hordePerfts: List[Perft]       = _
  var randomPerfts: List[Perft]      = _
  var trickyPerfts: List[Perft]      = _

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
    val x = perfts.map:
      Blackhole.consumeCPU(Work)
      _.calculate(variant)
    bh.consume(x)
    x

  extension (perft: Perft)
    def bench(variant: Variant): List[Result] =
      var situation = Fen.read(variant, perft.epd).get
      perft.cases.map: c =>
        import Perft.*
        Blackhole.consumeCPU(Work)
        Result(c.depth, situation.perft(c.depth), c.result)
