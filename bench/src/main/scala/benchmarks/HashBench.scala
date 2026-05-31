package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.format.pgn.Fixtures
import chess.{ Position, Hash, Replay }

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

  // EXPERIMENT-ONLY: which castling-hash implementation to exercise.
  // current = foldCastle (king lookup), corner = corner fast-path, ceiling = zero-king upper bound.
  @Param(Array("current", "corner", "ceiling"))
  var castlingMode: String = scala.compiletime.uninitialized
  private def modeInt: Int = castlingMode match
    case "corner" => 1
    case "ceiling" => 2
    case _ => 0

  var boards: List[Position] = scala.compiletime.uninitialized

  @Setup
  def setup() =
    val results =
      for
        results <- Fixtures.gamesForPerfTest.traverse(Replay.mainline(_))
        replays <- results.traverse(_.valid)
      yield replays.flatMap(_.moves).map(_.after)
    boards = results.toOption.get

  @Benchmark
  def hashes(bh: Blackhole) =
    val mode = modeInt
    val result = boards.map: x =>
      Blackhole.consumeCPU(Work)
      Hash.hashWith(x, mode)
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
