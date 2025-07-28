package benchmarks

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import cats.syntax.all.*
import chess.format.pgn.{ Fixtures, ParsedPgn, Parser, Pgn, PgnStr }

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class PgnBench:

  // the unit of CPU work per iteration
  private val Work: Long = 10

  var pgnStrs: List[PgnStr] = scala.compiletime.uninitialized
  var pgns: List[Pgn] = scala.compiletime.uninitialized
  var parsedPgn: List[ParsedPgn] = scala.compiletime.uninitialized

  @Setup
  def setup() =
    pgnStrs = Fixtures.gamesForPerfTest ++ Fixtures.wcc2023.map(PgnStr.apply)
    parsedPgn = pgnStrs.traverse(Parser.full).toOption.get
    pgns = pgnStrs.traverse(Parser.full).toOption.get.map(_.toPgn)

  @Benchmark
  def pgnFullParser(bh: Blackhole) =
    var games = this.pgnStrs
    var i = 0
    while i < games.size do
      val game = games(i)
      Blackhole.consumeCPU(Work)
      bh.consume(Parser.full(game))
      i += 1

  @Benchmark
  def pgnMainlineWithMetasParser(bh: Blackhole) =
    var games = this.pgnStrs
    var i = 0
    while i < games.size do
      val game = games(i)
      Blackhole.consumeCPU(Work)
      bh.consume(Parser.mainlineWithMetas(game))
      i += 1

  @Benchmark
  def pgnMainlineParser(bh: Blackhole) =
    var games = this.pgnStrs
    var i = 0
    while i < games.size do
      val game = games(i)
      Blackhole.consumeCPU(Work)
      bh.consume(Parser.mainline(game))
      i += 1

  @Benchmark
  def pgnRender(bh: Blackhole) =
    val result = pgns.map: x =>
      Blackhole.consumeCPU(Work)
      x.render
    bh.consume(result)
    result

  @Benchmark
  def pgnBuildAndRender(bh: Blackhole) =
    val result =
      parsedPgn.map: x =>
        Blackhole.consumeCPU(Work)
        x.toPgn.render
    bh.consume(result)
    result
