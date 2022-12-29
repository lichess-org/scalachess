package benchmarks

import org.openjdk.jmh.annotations._

import chess.PerftTestCase

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 10, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 3, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(2)
class PerftBench {

  var chess960Games = PerftTestCase.chess960
  @Benchmark
  def chess960() =
    chess960Games.map(_.calculate())

  var trickyGames = PerftTestCase.tricky
  @Benchmark
  def tricky() =
    trickyGames.map(_.calculate())

}
