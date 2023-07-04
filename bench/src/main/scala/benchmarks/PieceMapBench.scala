package benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import chess.{ Piece, Square }
import chess.format.*
import chess.bitboard.{ Board, FenFixtures }
import org.openjdk.jmh.infra.Blackhole
import chess.bitboard.Bitboard
import chess.Role
import chess.Color

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(3)
@Threads(value = 1)
class PieceMapBench:

  def parseFen(fen: EpdFen): Board =
    Fen.read(fen).map(_.board.board).getOrElse(throw RuntimeException("boooo"))

  var bs: List[Board] = _

  @Setup
  def setup() =
    bs = for
      str <- FenFixtures.fens
      board = parseFen(str)
    yield board

  def run(f: Board => Map[Square, Piece]) =
    bs.map: x =>
      Blackhole.consumeCPU(10)
      f(x)

  @Benchmark
  def current =
    run(_.pieceMapImpl1)

  @Benchmark
  def mapforSquaresViewMap =
    run(_.pieceMapImpl2)

  @Benchmark
  def flatMap =
    run(_.pieceMapImpl3)

  @Benchmark
  def byRole =
    run(_.pieceMapImpl4)

  @Benchmark
  def byRoleWithoutInnerFunction =
    run(_.pieceMapImpl5)

  @Benchmark
  def byRoleWithMutableMap =
    run(_.pieceMapImpl6)

  @Benchmark
  def foreachWithMutableMap =
    run(_.pieceMapImpl7)

  @Benchmark
  def foreachWithMutableMapAndNoGet =
    run(_.pieceMapImpl8)

  @Benchmark
  def foreachWithMapBuilder =
    run(_.pieceMapImpl9)

  @Benchmark
  def foreachWithMapBuilderAndNoGet =
    run(_.pieceMapImpl10)

extension (b: Board)
  // implementation at 15.4.0
  def pieceMapImpl1: Map[Square, Piece] =
    b.occupied.squares.view.map(s => (s, b.pieceAt(s).get)).toMap

  def pieceMapImpl2: Map[Square, Piece] =
    b.occupied.map(s => (s, b.pieceAt(s).get)).toMap

  def pieceMapImpl3: Map[Square, Piece] =
    b.occupied.flatMap(s => b.pieceAt(s).map(s -> _)).toMap

  def pieceMapImpl4: Map[Square, Piece] =
    def roleMap(role: Role, bb: Bitboard): Map[Square, Piece] =
      bb.map: s =>
        (s, Piece(Color.fromWhite(b.byColor.white.contains(s)), role))
      .toMap

    b.byRole.fold(Map.empty[Square, Piece]): (acc, role, rx) =>
      acc ++ roleMap(role, rx)

  def pieceMapImpl5: Map[Square, Piece] =
    b.byRole.fold(Map.empty): (acc, role, rx) =>
      acc ++
        rx.map: s =>
          (s, Color.fromWhite(b.byColor.white.contains(s)) - role)
        .toMap

  def pieceMapImpl6: Map[Square, Piece] =
    val m = collection.mutable.Map.empty[Square, Piece]
    b.byRole.fold(m): (acc, role, rx) =>
      acc.addAll(
        rx.map: s =>
          (s, Color.fromWhite(b.byColor.white.contains(s)) - role)
      )
    m.toMap

  def pieceMapImpl7: Map[Square, Piece] =
    val m = collection.mutable.Map.empty[Square, Piece]
    b.occupied.foreach:s =>
      m += s -> b.pieceAt(s).get
    m.toMap

  def pieceMapImpl8: Map[Square, Piece] =
    val m = collection.mutable.Map.empty[Square, Piece]
    b.occupied.foreach:s =>
      m ++= b.pieceAt(s).map(s -> _)
    m.toMap

  def pieceMapImpl9: Map[Square, Piece] =
    val m = Map.newBuilder[Square, Piece]
    b.occupied.foreach:s =>
      m += s -> b.pieceAt(s).get
    m.result

  def pieceMapImpl10: Map[Square, Piece] =
    val m = Map.newBuilder[Square, Piece]
    b.occupied.foreach:s =>
      m ++= b.pieceAt(s).map(s -> _)
    m.result
