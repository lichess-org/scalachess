package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.format.pgn.{ Fixtures, Reader }
import chess.{ Position, Hash }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class HashBench:

  // the unit of CPU work per iteration
  private val Work: Long = 10

  var boards: List[Position] = scala.compiletime.uninitialized

  @Setup
  def setup() =
    val results = for
      results <- Fixtures.gamesForPerfTest.traverse(Reader.full(_))
      replays <- results.traverse(_.valid)
    yield replays.flatMap(_.moves).map(_.boardAfter)
    boards = results.toOption.get

  @Benchmark
  def hashes(bh: Blackhole) =
    val result = boards.map: x =>
      Blackhole.consumeCPU(Work)
      Hash(x)
    bh.consume(result)

  @Benchmark
  def repetition5(bh: Blackhole) =
    val result = boards.map: x =>
      Blackhole.consumeCPU(Work)
      x.history.fivefoldRepetition
    bh.consume(result)

  @Benchmark
  def repetition3(bh: Blackhole) =
    val result = boards.map: x =>
      Blackhole.consumeCPU(Work)
      x.history.threefoldRepetition
    bh.consume(result)
