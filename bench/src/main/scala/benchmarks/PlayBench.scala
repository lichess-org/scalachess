package benchmarks

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

import cats.syntax.all.*

import chess.Square.*
import chess.format.pgn.Fixtures
import chess.format.pgn.SanStr
import chess.variant.Standard
import chess.{ Mode => _, * }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Threads(value = 1)
class PlayBench:

  var standard = Game(Board init chess.variant.Standard, White)

  @Benchmark
  def divider() =
    var moves      = Fixtures.fromProd2
    val gameReplay = Replay.boards(SanStr from moves.split(' ').toList, None, Standard).toOption.get
    Divider(gameReplay)

  @Benchmark
  def replay() =
    var nb    = 500
    var games = Fixtures.prod500standard
    var gameMoves = (games take nb).map { g =>
      SanStr from g.split(' ').toList
    }
    gameMoves foreach { moves =>
      Replay.gameMoveWhileValid(moves, chess.format.Fen.initial, chess.variant.Standard)
    }

  @Benchmark
  def play() =
    standard.playMoves(
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

  extension (game: Game)
    def as(color: Color): Game = game.withPlayer(color)

    def playMoves(moves: (Square, Square)*): Either[ErrorStr, Game] = playMoveList(moves)

    def playMoveList(moves: Iterable[(Square, Square)]): Either[ErrorStr, Game] =
      moves.toList.foldM(game):
        case (game, (o, d)) => game(o, d).map(_._1)
