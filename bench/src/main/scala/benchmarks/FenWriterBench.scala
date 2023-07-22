package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.variant.*
import chess.{ Mode as _, * }
import chess.format.EpdFen
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

  private[this] val Work: Long = 5

  @Param(Array("100"))
  var games: Int = _

  var threecheckInput: List[Game]  = _
  var antichessInput: List[Game]   = _
  var atomicInput: List[Game]      = _
  var crazyhouseInput: List[Game]  = _
  var racingkingsInput: List[Game] = _
  var hordeInput: List[Game]       = _
  var randomInput: List[Game]      = _
  var trickyInput: List[Game]      = _

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

  private def bench(sits: List[Game])(bh: Blackhole) =
    val x = sits.map: x =>
      Blackhole.consumeCPU(Work)
      Fen.write(x)
    bh.consume(x)

  private def makeSituations(perfts: List[Perft], variant: Variant, games: Int): List[Game] =
    perfts
      .take(games)
      .map(p => Fen.read(variant, p.epd).getOrElse(throw RuntimeException("boooo")))
      .map(s => Game(s, ply = Ply.initial))
