package benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.format.pgn.{ Fixtures, Reader }
import chess.MoveOrDrop.situationAfter
import chess.{ Hash, Situation }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class HashBench:

  // the unit of CPU work per iteration
  private[this] val Work: Long = 10

  var situations: List[Situation] = _

  @Setup
  def setup() =
    var results = for
      results <- Fixtures.gamesForPerfTest.traverse(Reader.full(_))
      replays <- results.traverse(_.valid)
    yield replays.flatMap(_.moves).map(_.situationAfter)
    situations = results.toOption.get

  @Benchmark
  def hashes(bh: Blackhole) =
    var result = situations.map: x =>
      Blackhole.consumeCPU(Work)
      Hash(x)
    bh.consume(result)
