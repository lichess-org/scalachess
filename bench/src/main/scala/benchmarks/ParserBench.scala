package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.format.pgn.{ Fixtures, Parser, PgnStr }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class ParserBench:

  // the unit of CPU work per iteration
  private[this] val Work: Long = 10

  var games: List[PgnStr] = _

  @Setup
  def setup() =
    games = Fixtures.gamesForPerfTest

  @Benchmark
  def pgnParser(bh: Blackhole) =
    var result = games.traverse { x =>
      Blackhole.consumeCPU(Work)
      Parser.full(x)
    }
    bh.consume(result)
    result
