package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import chess.variant.*
import chess.format.FullFen
import chess.perft.Perft
import chess.format.Fen

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class FenReaderBench:

  private val Work: Long = 5

  @Param(Array("100"))
  var games: Int = scala.compiletime.uninitialized

  var threecheckInput: List[(Variant, FullFen)]  = scala.compiletime.uninitialized
  var antichessInput: List[(Variant, FullFen)]   = scala.compiletime.uninitialized
  var atomicInput: List[(Variant, FullFen)]      = scala.compiletime.uninitialized
  var crazyhouseInput: List[(Variant, FullFen)]  = scala.compiletime.uninitialized
  var racingkingsInput: List[(Variant, FullFen)] = scala.compiletime.uninitialized
  var hordeInput: List[(Variant, FullFen)]       = scala.compiletime.uninitialized
  var randomInput: List[(Variant, FullFen)]      = scala.compiletime.uninitialized
  var trickyInput: List[(Variant, FullFen)]      = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =
    threecheckInput = makeFens(Perft.threeCheckPerfts, ThreeCheck, games)
    antichessInput = makeFens(Perft.antichessPerfts, Antichess, games)
    atomicInput = makeFens(Perft.atomicPerfts, Atomic, games)
    crazyhouseInput = makeFens(Perft.crazyhousePerfts, Crazyhouse, games)
    racingkingsInput = makeFens(Perft.racingkingsPerfts, RacingKings, games)
    hordeInput = makeFens(Perft.hordePerfts, Horde, games)
    randomInput = makeFens(Perft.randomPerfts, Chess960, games)
    trickyInput = makeFens(Perft.trickyPerfts, Chess960, games)

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

  private def bench(sits: List[(Variant, FullFen)])(bh: Blackhole) =
    val x = sits.map: x =>
      Blackhole.consumeCPU(Work)
      Fen.read(x._1, x._2)
    bh.consume(x)

  private def makeFens(perfts: List[Perft], variant: Variant, games: Int): List[(Variant, FullFen)] =
    perfts
      .take(games)
      .map(p => variant -> p.epd)
