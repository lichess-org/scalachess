package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.{ FullMoveNumber, Situation }
import chess.variant.Chess960
import chess.{ Mode as _, * }
import chess.format.{ Fen, FullFen, BinaryFen }
import chess.perft.Perft

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class BinaryFenBench:

  // the unit of CPU work per iteration
  private val Work: Long = 10

  @Param(Array("10", "100", "1000"))
  var games: Int                              = scala.compiletime.uninitialized
  var sits: List[Situation.AndFullMoveNumber] = scala.compiletime.uninitialized
  var fens: List[BinaryFen]                   = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit =
    sits = makeSituations(Perft.randomPerfts, games)
    fens = sits.map(BinaryFen.write)

  private def makeSituations(perfts: List[Perft], games: Int): List[Situation.AndFullMoveNumber] =
    perfts
      .take(games)
      .flatMap(x => Fen.read(Chess960, x.epd))
      .map(Situation.AndFullMoveNumber(_, FullMoveNumber(1)))

  @Benchmark
  def write(bh: Blackhole) =
    val games = this.sits
    var i     = 0
    while i < games.size do
      val game = games(i)
      Blackhole.consumeCPU(Work)
      bh.consume(BinaryFen.write(game))

  @Benchmark
  def read(bh: Blackhole) =
    val games = this.fens
    var i     = 0
    while i < games.size do
      val fen = games(i)
      Blackhole.consumeCPU(Work)
      bh.consume(fen.read)
      i += 1
