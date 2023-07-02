package benchmarks

import org.openjdk.jmh.annotations._

import cats.syntax.all.*

import java.util.concurrent.TimeUnit
import chess.format.pgn.Fixtures
import chess.format.pgn.Reader
import chess.MoveOrDrop.move
import chess.Hash

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Threads(value = 1)
class HashBench:

  var games = Fixtures.gamesForPerfTest.traverse(Reader.full(_)).toOption.get.traverse(_.valid).toOption.get

  var situations = games.flatMap(_.moves).flatMap(_.move).map(_.situationAfter)

  @Benchmark
  def hashes() =
    situations.map(Hash(_))
