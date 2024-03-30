package benchmarks

import org.openjdk.jmh.annotations.*

import cats.syntax.all.*
import java.util.concurrent.TimeUnit
import chess.format.{ FullFen, Fen }
import chess.variant.Horde

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class InsufficientMaterialBench:

  var hordeGames = List(
    "k7/ppP5/brp5/8/8/8/8/8 b - -",
    "8/2k5/3q4/8/8/8/1P6/8 b - -",
    "8/2k5/3q4/8/8/8/1P6/8 w - -",
    "r7/2Bb4/q3k3/8/8/3q4/8/5qqr b - -",
    "8/2k5/3q4/8/8/1Q6/8/8 b - -",
    "8/2k5/3q4/8/8/1Q6/8/8 w - -",
    "8/2k5/3q4/8/8/1B2N3/8/8 b - -",
    "8/2k5/3q4/8/8/1B2N3/8/8 w - -",
    "8/2k5/3q4/8/8/3B4/4NB2/8 b - -",
    "8/5k2/7q/7P/6rP/6P1/6P1/8 b - - 0 52",
    "8/p7/pk6/P7/P7/8/8/8 b - -",
    "QNBRRBNQ/PPpPPpPP/P1P2PkP/8/8/8/8/8 b - -",
    "b7/pk6/P7/P7/8/8/8/8 b - - 0 1",
    "8/1b5r/1P6/1Pk3q1/1PP5/r1P5/P1P5/2P5 b - - 0 52",
    "7B/6k1/8/8/8/8/8/8 b - -",
    "k7/5p2/4p2P/3p2P1/2p2P2/1p2P2P/p2P2P1/2P2P2 w - - 0 1",
    "8/N7/8/8/8/8/bqnnbqbr/k7 b - - 0 1",
    "8/6PP/8/8/8/8/3npqrn/7k b - - 0 1",
    "8/P1P5/8/8/8/8/bbnb4/k7 b - - 0 1",
    "8/6PP/8/8/8/8/5rrb/7k b - - 0 1"
  ).map(FullFen(_)).map(Fen.read(Horde, _).get)

  @Benchmark
  def horde() =
    hordeGames.map: situation =>
      situation.variant.isInsufficientMaterial(situation.board)
