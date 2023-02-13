package benchmarks

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

import chess.format.pgn.{ Fixtures, SanStr }
import chess.Replay

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 10, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 3, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(2)
class ReplayBench:

  val games: List[List[SanStr]] = Fixtures.prod500standard.map(_.split(" ").toList.map(SanStr(_)))

  @Benchmark
  def situations() =
    games foreach { moves =>
      Replay.situations(moves, None, chess.variant.Standard)
    }
