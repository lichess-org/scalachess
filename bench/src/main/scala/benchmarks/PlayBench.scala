package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.Square.*
import chess.format.pgn.{ Fixtures, SanStr }
import chess.{ Mode as _, * }

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
  var standard: Game                  = scala.compiletime.uninitialized

  def gameReplay(sans: String) =
    Position.standard.playBoards(SanStr.from(sans.split(' ')).toList).toOption.get

  @Setup
  def setup() =
    dividerGames = Fixtures.prod500standard.map(gameReplay)

    var nb    = 50
    var games = Fixtures.prod500standard
    gameMoves = games.take(nb).map(g => SanStr.from(g.split(' ').toList))

    standard = Game(Position.init(chess.variant.Standard, White))

  @Benchmark
  def divider(bh: Blackhole) =
    var result = dividerGames.map: x =>
      Blackhole.consumeCPU(Work)
      Divider(x)
    bh.consume(result)
    result

  @Benchmark
  def replay(bh: Blackhole) =
    var result = gameMoves.map: moves =>
      Blackhole.consumeCPU(Work)
      Replay.gameMoveWhileValid(moves, chess.format.Fen.initial, chess.variant.Standard)
    bh.consume(result)
    result

  @Benchmark
  def play(bh: Blackhole) =
    var result = standard.playMoves(
      bh,
      E2 -> E4,
      D7 -> D5,
      E4 -> D5,
      D8 -> D5,
      B1 -> C3,
      D5 -> A5,
      D2 -> D4,
      C7 -> C6,
      G1 -> F3,
      C8 -> G4,
      C1 -> F4,
      E7 -> E6,
      H2 -> H3,
      G4 -> F3,
      D1 -> F3,
      F8 -> B4,
      F1 -> E2,
      B8 -> D7,
      A2 -> A3,
      E8 -> C8,
      A3 -> B4,
      A5 -> A1,
      E1 -> D2,
      A1 -> H1,
      F3 -> C6,
      B7 -> C6,
      E2 -> A6
    )
    bh.consume(result)
    result

  extension (game: Game)
    def as(color: Color): Game = game.withPlayer(color)

    def playMoves(bh: Blackhole, moves: (Square, Square)*): Either[ErrorStr, Game] = playMoveList(bh, moves)

    def playMoveList(bh: Blackhole, moves: Iterable[(Square, Square)]): Either[ErrorStr, Game] =
      moves.toList.foldM(game):
        case (game, (o, d)) =>
          // because possible moves are asked for player highlight
          // before the move is played (on initial board)
          Blackhole.consumeCPU(Work)
          var result = game.position.destinations
          bh.consume(result)
          game(o, d).map(_._1)
