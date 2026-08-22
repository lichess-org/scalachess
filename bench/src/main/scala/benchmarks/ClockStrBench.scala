package benchmarks

import cats.syntax.all.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import chess.format.pgn.Move
import scalalib.model.Seconds

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class ClockStrBench:

  private val Work: Long = 5

  var seconds = List(0, 9, 37, 62, 95, 121, 328, 633, 1285, 3600, 10_000).map(Seconds(_))

  @Benchmark
  def formatPgnSeconds(bh: Blackhole) =
    Blackhole.consumeCPU(Work)
    seconds.map(Move.formatPgnSeconds(_, StringBuilder(16))).toString

  var clocks = (none, none) :: (for
    clk <- seconds
    emt <- List(0.some, 3.some, 11.some, 63.some, 1923.some, none)
  yield (clk.some, emt.map(Seconds(_))))

  @Benchmark
  def clockString(bh: Blackhole) =
    Blackhole.consumeCPU(Work)
    clocks.map(Move.clockString)
