package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.Square.*
import chess.format.pgn.{ Fixtures, SanStr }
import chess.*
import chess.variant.Standard

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class PlayBench:

  // the unit of CPU work per iteration
  private val Work: Long = 10

  var dividerGames: List[List[Board]] = scala.compiletime.uninitialized
  var gameMoves: List[List[SanStr]]   = scala.compiletime.uninitialized

  def gameReplay(sans: String) =
    Standard.initialPosition.playBoards(SanStr.from(sans.split(' ')).toList).toOption.get

  @Setup
  def setup() =
    dividerGames = Fixtures.prod500standard.map(gameReplay)

    var nb    = 50
    var games = Fixtures.prod500standard
    gameMoves = games.take(nb).map(g => SanStr.from(g.split(' ').toList))

  @Benchmark
  def divider(bh: Blackhole) =
    var result = dividerGames.map: x =>
      Blackhole.consumeCPU(Work)
      Divider(x)
    bh.consume(result)
    result

  @Benchmark
  def gameMoveWhileValid(bh: Blackhole) =
    val games = this.gameMoves
    var i     = 0
    while i < games.size do
      val moves = games(i)
      Blackhole.consumeCPU(Work)
      bh.consume(Replay.gameMoveWhileValid(moves, chess.format.Fen.initial, chess.variant.Standard))
      i += 1

  @Benchmark
  def playMoveOrDropWithPly(bh: Blackhole) =
    val games = this.gameMoves
    var i     = 0
    while i < games.size do
      val moves = games(i)
      Blackhole.consumeCPU(Work)
      val init = chess.Position.AndFullMoveNumber(chess.variant.Standard, chess.format.Fen.initial.some)
      bh.consume(init.position.play(moves, init.ply)(step => chess.MoveOrDrop.WithPly(step.move, step.ply)))
      i += 1
