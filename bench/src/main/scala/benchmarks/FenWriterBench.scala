package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import chess.variant.*
import chess.{ Mode as _, * }
import chess.perft.Perft
import chess.format.Fen

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class FenWriterBench:

  private val Work: Long = 5

  @Param(Array("100"))
  var games: Int = scala.compiletime.uninitialized

  var threecheckInput: List[Game]  = scala.compiletime.uninitialized
  var antichessInput: List[Game]   = scala.compiletime.uninitialized
  var atomicInput: List[Game]      = scala.compiletime.uninitialized
  var crazyhouseInput: List[Game]  = scala.compiletime.uninitialized
  var racingkingsInput: List[Game] = scala.compiletime.uninitialized
  var hordeInput: List[Game]       = scala.compiletime.uninitialized
  var randomInput: List[Game]      = scala.compiletime.uninitialized
  var trickyInput: List[Game]      = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =
    threecheckInput = makeBoards(Perft.threeCheckPerfts, ThreeCheck, games)
    antichessInput = makeBoards(Perft.antichessPerfts, Antichess, games)
    atomicInput = makeBoards(Perft.atomicPerfts, Atomic, games)
    crazyhouseInput = makeBoards(Perft.crazyhousePerfts, Crazyhouse, games)
    racingkingsInput = makeBoards(Perft.racingkingsPerfts, RacingKings, games)
    hordeInput = makeBoards(Perft.hordePerfts, Horde, games)
    randomInput = makeBoards(Perft.randomPerfts, Chess960, games)
    trickyInput = makeBoards(Perft.trickyPerfts, Chess960, games)

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

  private def bench(sits: List[Game])(bh: Blackhole) =
    val x = sits.map: x =>
      Blackhole.consumeCPU(Work)
      Fen.write(x)
    bh.consume(x)

  private def makeBoards(perfts: List[Perft], variant: Variant, games: Int): List[Game] =
    perfts
      .take(games)
      .map(p => Fen.read(variant, p.epd).getOrElse(throw RuntimeException("boooo")))
      .map(s => Game(s, ply = Ply.initial))
