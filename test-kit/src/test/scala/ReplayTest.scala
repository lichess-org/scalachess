package chess

import cats.syntax.all.*
import chess.format.pgn.Fixtures.*
import chess.format.pgn.{ PgnStr, SanStr }
import chess.variant.{ Chess960, FromPosition, Standard }
import snapshot4s.generated.snapshotConfig
import snapshot4s.munit.SnapshotAssertions

import format.{ FullFen, Fen }

class ReplayTest extends MunitExtensions with SnapshotAssertions:

  test("replay from position close chess"):
    val fen = FullFen("""8/rnbqkbnr/pppppppp/8/8/PPPPPPPP/RNBQKBNR/8 w - - 0 1""")
    val moves = SanStr.from("""d4 d5 Nf4 Nf5 g4 g5 gxf5 exf5""".split(' ').toList)
    val init = Position.AndFullMoveNumber(FromPosition, fen)
    assertRight(init.playMoves(moves)): moves =>
      assertEquals(moves.size, 8)
      assertEquals(moves(1).toSanStr, "d5")

  test("replay errors should keep order"):
    val fen = FullFen("""8/rnbqkbnr/pppppppp/8/8/PPPPPPPP/RNBQKBNR/8 w - - 0 1""")
    val moves = SanStr.from("""d4 d5 Nf3""".split(' ').toList)
    val init = Position.AndFullMoveNumber(FromPosition, fen)
    assertRight(init.playWhileValid(moves, init.ply)):
      case (_, moves, Some(err)) =>
        assertEquals(moves.size, 2)
        assertEquals(moves(1).toSanStr, "d5")
        assertEquals(moves(0).toSanStr, "d4")
        assertEquals(err, ErrorStr("Cannot play Nf3 at move 2 by white"))

  test("chess960 castlings"):
    val sans = SanStr.from("e4 e5 Ne3 f6 Nb3 Nb6 O-O-O Ne6 Ba6 O-O-O Nd5 Nxd5 Bxb7+ Kb8".split(' ')).toVector
    val fen = Fen.Full("nrknbbqr/pppppppp/8/8/8/8/PPPPPPPP/NRKNBBQR w KQkq - 0 1")
    val init = Position.AndFullMoveNumber(Chess960, fen)
    assertRight(init.playMoves(sans)): moves =>
      assertEquals(moves.size, sans.size)

  test("castling always 960 notation"):
    val sans =
      SanStr.from("d4 Nf6 c4 g6 Nc3 Bg7 e4 d6 f3 O-O Be3 e5 d5 Nh5 Qd2 Qh4+ g3 Qe7 O-O-O".split(' ')).toVector
    val init = Position.AndFullMoveNumber(Standard, Fen.Full.initial)
    assertRight(init.playMoves(sans)): moves =>
      assertEquals(moves.size, sans.size)
      assertEquals(moves(9).toUci.uci, "e8h8")
      assertEquals(moves(18).toUci.uci, "e1a1")

  test("replay from fen then castle"):
    val fen = Fen.Full("2bqkb1r/1pp1ppp1/7r/pN2p2p/3PP3/P3P3/1PP1B1PP/R2Q1RK1 w k - 3 13")
    val moves = SanStr.from("dxe5 Qxd1 Raxd1 Rc6 Rd2 e6 Rfd1 Be7 Na7 O-O".split(" ")).toList
    val init = Position.AndFullMoveNumber(Standard, fen)
    assertRight(init.playMoves(moves)): steps =>
      assertEquals(steps.size, moves.size)

  test("antichess replay invalid san"):
    val fen = FullFen("r1b1k3/pp3p2/n2pp3/2p5/4p3/2P2N2/P4n2/R7 b - - 0 17")
    val init = Position.AndFullMoveNumber(variant.Antichess, fen)
    assert(init.play(SanStr("c4")).isLeft)
    assert(init.play(SanStr("exf3")).isRight)

  test("Castling should not be possible when in check"):
    val fen = FullFen("r1bq1rk1/ppp2ppp/2n5/8/1bB1Pp2/5N2/PPP1Q1PP/R1B1K2R w KQ - 1 10")
    val init = Position.AndFullMoveNumber(variant.Standard, fen)
    assert(init.play(SanStr("O-O")).isLeft)
    assert(init.playMoves(List(SanStr("O-O"))).isLeft)
    assert(init.play(SanStr("Qd2")).isRight)


  // format: off
  private val fixtures = PgnStr.from:
    List( clonoNoExoticNotation, invisibleChar, fromProd1, fromProd2, promoteRook, castleCheck1, castleCheck2, complete960,
      "\n" + complete960 + "\n", fromTcec, fromChessProgrammingWiki, commentsAndVariations, bySmartChess, invalidVariant,
      fromLichessBadPromotion, chessbaseArrows, atomicRegression, lichobile, crazyhouse1, crazyhouse2, crazyhouseNoVariantTag,
      withDelimiters, withDelimitersOnNewLines, chessComCrazyhouse, fromPosProdCloseChess, fromPositionEmptyFen, caissa, festivalFigueira
    ) ++ raws
  // format: on

  test("Replay.mainline to fens snapshot"):
    val x = fixtures.traverse(Replay.mainline).toOption.get.writeFen
    assertFileSnapshot(x, "replay/mainline_fen.txt")

  test("Replay.mainline preserves initial ply"):
    Replay
      .mainline(PgnStr(caissa))
      .assertRight:
        case Replay.Result(replay, None) =>
          assertEquals(replay.setup.startedAtPly, 43)

  extension (xs: List[Replay.Result])
    def writeFen: String =
      xs.map(_.replay.writeFen)
        .mkString("\n")

  extension (replay: Replay)
    // write fen for every positions in the replay, including the last position twice
    // to test that replay include the initial position and the final position
    def writeFen: String =
      (replay.setup.position +: replay.chronoMoves.map(_.after) :+ replay.state.position)
        .map(Fen.write)
        .mkString("\n")
