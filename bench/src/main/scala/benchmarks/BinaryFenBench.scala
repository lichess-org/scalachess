package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.{ FullMoveNumber, Situation }
import chess.format.BinaryFen
import chess.variant.Standard

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

  private val binary = BinaryFen(
    Array(
      0xff.toByte,
      0xff.toByte,
      0x00.toByte,
      0x00.toByte,
      0x10.toByte,
      0x00.toByte,
      0xef.toByte,
      0xff.toByte,
      0x2d.toByte,
      0x84.toByte,
      0x4a.toByte,
      0xd2.toByte,
      0x00.toByte,
      0x00.toByte,
      0x00.toByte,
      0x00.toByte,
      0x11.toByte,
      0x11.toByte,
      0x11.toByte,
      0x11.toByte,
      0x3e.toByte,
      0x95.toByte,
      0x5f.toByte,
      0xe3.toByte
    )
  )

  import chess.variant.*
  import chess.{ Mode as _, * }
  import chess.format.FullFen
  import chess.perft.Perft
  import chess.format.Fen

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
