package benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.infra.Blackhole
import chess.format.*
import chess.perft.Perft
import chess.variant.*
import chess.Position

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

  var threecheckInput: List[Position] = scala.compiletime.uninitialized
  var antichessInput: List[Position] = scala.compiletime.uninitialized
  var atomicInput: List[Position] = scala.compiletime.uninitialized
  var crazyhouseInput: List[Position] = scala.compiletime.uninitialized
  var racingkingsInput: List[Position] = scala.compiletime.uninitialized
  var hordeInput: List[Position] = scala.compiletime.uninitialized
  var randomInput: List[Position] = scala.compiletime.uninitialized
  var trickyInput: List[Position] = scala.compiletime.uninitialized

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

  private def bench(sits: List[Position])(bh: Blackhole) =
    val x = sits.map: x =>
      Blackhole.consumeCPU(Work)
      x.destinations
    bh.consume(x)

  private def makeBoards(perfts: List[Perft], variant: Variant, games: Int): List[Position] =
    perfts
      .take(games)
      .map(p => Fen.read(variant, p.epd).getOrElse(throw RuntimeException("boooo")))
