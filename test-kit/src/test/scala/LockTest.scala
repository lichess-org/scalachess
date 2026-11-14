package chess

import chess.Square.*
import chess.format.{ Fen, FullFen }
import chess.variant.Standard

class LockTest extends ChessTest:

  // Fen.readWithMoveNumber keeps the FEN's halfmove clock; Fen.read (and fenToGame) sets it to 0.
  private def at(fen: String) = Fen.readWithMoveNumber(Standard, FullFen(fen)).get.position

  // Final positions of Lichess games lost on time and scored as wins, with the halfmove clock set to
  // 0 or 1 so that autoDraw asks: dead positions.
  List(
    "8/8/4k3/3p2p1/1p1P1pP1/1P3P2/8/3K4 b - - 0 92",
    "3k4/8/3p4/2pP1p1p/p1P2PpP/P2K2P1/8/8 b - - 1 56",
    "8/8/4k3/p2p2p1/P2P2P1/2K5/8/8 w - - 0 75"
  ).foreach: fen =>
    test(s"locked: $fen"):
      assert(at(fen).autoDraw)
      assert(at(fen).end)

  test("asked after a capture or a pawn move and one move later only"):
    assert(at("8/8/4k3/3p2p1/1p1P1pP1/1P3P2/8/3K4 b - - 1 92").autoDraw)
    assertNot(at("8/8/4k3/3p2p1/1p1P1pP1/1P3P2/8/3K4 b - - 2 92").autoDraw)

  test("the pawn move that locks"):
    fenToGame(FullFen("8/8/4k3/3p2p1/1p1P1pP1/1P6/5P2/3K4 w - - 3 91"), Standard)
      .playMoves(F2 -> F3)
      .assertRight: g =>
        assert(g.position.autoDraw)

  test("a king steps out of a pawn's check into a lock"):
    val game = fenToGame(FullFen("8/8/1k6/p2p2p1/PK1P2Pp/7P/8/8 w - - 0 49"), Standard)
    assertNot(game.position.autoDraw) // in the pawn's check
    game
      .playMoves(B4 -> C3)
      .assertRight: g =>
        assert(g.position.autoDraw)

  test("not locked: a knight on the board"):
    assertNot(at("8/8/4k3/3p2p1/1p1P1pP1/1P3P2/8/3KN3 b - - 0 92").autoDraw)
  test("not locked: a pawn can still capture"):
    assertNot(at("8/8/4k3/3p2p1/1p1P1pP1/1P2P3/8/3K4 b - - 0 92").autoDraw)
