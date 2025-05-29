package chess

import cats.syntax.all.*
import chess.format.Fen
import chess.format.pgn.{ Fixtures, PgnStr, SanStr }
import snapshot4s.generated.snapshotConfig
import snapshot4s.munit.SnapshotAssertions

import macros.*
import format.Uci

class CanPlayTest extends MunitExtensions with SnapshotAssertions:

  test("playPositions from standard prod games"):
    val nb    = 10
    val games = Fixtures.prod500standard
    val result = games
      .take(nb)
      .map(g => SanStr.from(g.split(' ').toList))
      .flatTraverse(moves => Position.standard.playPositions(moves))
    assertFileSnapshot(result.writeFen, "canplay/playPositions_standard.txt")

  test("playPositions racing kings"):
    val result = chess.variant.RacingKings.initialPosition
      .playPositions(SanStr.from("Be3 Ne4 Rg3 Nxe3 Rxe3".split(" ")).toList)
    assertFileSnapshot(result.writeFen, "canplay/playPositions_racing_kings.txt")

  test("validate from prod games"):
    val games = Fixtures.prod500standard
    val result = games
      .map(g => SanStr.from(g.split(' ').toList))
      .traverse_(moves => Position.standard.validate(moves))
    assertEquals(result, ().asRight)

  test("validate return left on invalid move"):
    val position = Position.standard
    val moves    = List(uci"e2e4", uci"e7e5", uci"g1f3", uci"b8b6")
    val result   = position.validate(moves)
    assert(result.isLeft)

  test("forwad from prod games"):
    val nb    = 10
    val games = Fixtures.prod500standard
    val result = games
      .take(nb)
      .map(g => SanStr.from(g.split(' ').toList))
      .traverse(moves => Position.standard.forward(moves))
    assertFileSnapshot(result.writeFen, "canplay/forward_standard.txt")

  test("playWhileValid and playWhileValidReverse from prod games"):
    val sans = SanStr.from(Fixtures.fromProd2.split(' ').toList)
    val x    = Position.standard.playWhileValid(sans, Ply.initial)(_.move.toUci).toOption.get
    val y    = Position.standard.playWhileValidReverse(sans, Ply.initial)(_.move.toUci).toOption.get
    assertEquals(x.moves, y.moves.reverse)
    assertEquals(x.error, y.error)
    assertEquals(x.state.board, y.state.board)

  /*============== Error Messages ==============*/

  test("error message for white"):
    val sans  = List(SanStr("Nf7"))
    val error = Position.standard.playPositions(sans).swap.toOption.get
    assertEquals(error, ErrorStr("Cannot play Nf7 at move 1 by white"))

  test("error message for black"):
    val sans  = List("e4", "e4").map(SanStr(_))
    val error = Position.standard.playPositions(sans).swap.toOption.get
    assertEquals(error, ErrorStr("Cannot play e4 at move 1 by black"))

  test("more error message"):
    val pgn = PgnStr(
      "e3 Nc6 d4 Nf6 c3 e5 dxe5 Nxe5 Bb5 a6 Ba4 b5 Bb3 d5 e4 dxe4 f4 Qxd1+ Kxd1 Nd3 Be3 Ng4 Bd4 Ngf2+ Bxf2 Nxf2+ Ke1 Nxh1 Bd5 Ra7 Bc6+ Kd8 Bxe4 Bd6 g3 Re8 Nd2 f5 Ne2 fxe4 Kf1 e3 Kg2 exd2 Rxh1 Bb7+ Kf2 Bg3+ Kf3 d1=Q#"
    )
    Replay
      .mainline(pgn)
      .assertRight:
        case Replay.Result(_, Some(error)) =>
          assertEquals(error, ErrorStr("Cannot play Bg3 at move 24 by black"))

  extension [E](either: Either[E, List[Position]])
    private def writeFen: String =
      either.toOption.get
        .map(Fen.write)
        .mkString("\n")
