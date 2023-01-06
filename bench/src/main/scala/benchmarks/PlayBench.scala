package benchmarks

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

import cats.data.Validated
import cats.syntax.option.*

import chess.Pos.*
import chess.format.pgn.Fixtures
import chess.format.pgn.SanStr
import chess.variant.Standard
import chess.{Mode => _ , *}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 10, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 3, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(2)
class PlayBench:

  var standard = Game(Board init chess.variant.Standard, White)

  @Benchmark
  def divider() =
    var moves       = Fixtures.fromProd2
    val gameReplay  = Replay.boards(SanStr from moves.split(' ').toList, None, Standard).toOption.get
    Divider(gameReplay)

  @Benchmark
  def replay() =
    var nb = 500
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

    def playMoves(moves: (Pos, Pos)*): Validated[String, Game] = playMoveList(moves)

    def playMoveList(moves: Iterable[(Pos, Pos)]): Validated[String, Game] =
      val vg = moves.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, move) =>
        // vg foreach { x =>
        // println(s"------------------------ ${x.turns} = $move")
        // }
        // because possible moves are asked for player highlight
        // before the move is played (on initial situation)
        val _ = vg map { _.situation.destinations }
        val ng = vg flatMap { g =>
          g(move._1, move._2) map (_._1)
        }
        ng
      }
      // vg foreach { x => println("========= PGN: " + x.pgnMoves) }
      vg
