package chess

import chess.format.FullFen
import chess.format.pgn.SanStr

import variant.Horde

class HordeVariantTest extends ChessTest:

  test("Must not be insufficient winning material for horde with only 1 pawn left"):
    val position = FullFen("k7/ppP5/brp5/8/8/8/8/8 b - -")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.opponentHasInsufficientMaterial)

  test("Must recognise insufficient winning material for horde with only 1 pawn left"):
    val position = FullFen("8/2k5/3q4/8/8/8/1P6/8 b - -")
    val game     = fenToGame(position, Horde)
    assert(game.position.opponentHasInsufficientMaterial)

  test("Must not be insufficient winning material for king with only 1 pawn left"):
    val position = FullFen("8/2k5/3q4/8/8/8/1P6/8 w - -")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.opponentHasInsufficientMaterial)

  test("Must recognise insufficient winning material for horde with only 1 bishop left"):
    val position = FullFen("r7/2Bb4/q3k3/8/8/3q4/8/5qqr b - -")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.autoDraw)
    assertNot(game.position.end)
    assert(game.position.opponentHasInsufficientMaterial)

  test("Must recognise insufficient winning material for horde with only 1 queen left"):
    val position = FullFen("8/2k5/3q4/8/8/1Q6/8/8 b - -")
    val game     = fenToGame(position, Horde)
    assert(game.position.opponentHasInsufficientMaterial)

  test("Must not be insufficient winning material for king with only 1 queen left"):
    val position = FullFen("8/2k5/3q4/8/8/1Q6/8/8 w - -")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.opponentHasInsufficientMaterial)

  test("Must recognise insufficient winning material for horde with only 2 minor pieces left"):
    val position = FullFen("8/2k5/3q4/8/8/1B2N3/8/8 b - -")
    val game     = fenToGame(position, Horde)
    assert(game.position.opponentHasInsufficientMaterial)

  test("Must not be insufficient winning material for king with only 2 minor pieces left"):
    val position = FullFen("8/2k5/3q4/8/8/1B2N3/8/8 w - -")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.opponentHasInsufficientMaterial)

  test("Must not be insufficient winning material for horde with 3 minor pieces left"):
    val position = FullFen("8/2k5/3q4/8/8/3B4/4NB2/8 b - -")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.opponentHasInsufficientMaterial)

  test("Must not be insufficient winning material for king with queen and rook left"):
    val position = FullFen("8/5k2/7q/7P/6rP/6P1/6P1/8 b - - 0 52")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.opponentHasInsufficientMaterial)
    assertNot(game.position.autoDraw)

  test("Must auto-draw in simple pawn fortress"):
    val position = FullFen("8/p7/pk6/P7/P7/8/8/8 b - -")
    val game     = fenToGame(position, Horde)
    assert(game.position.autoDraw)
    assert(game.position.opponentHasInsufficientMaterial)

  test("Must auto-draw if horde is stalemated and only king can move"):
    val position = FullFen("QNBRRBNQ/PPpPPpPP/P1P2PkP/8/8/8/8/8 b - -")
    val game     = fenToGame(position, Horde)
    assert(game.position.autoDraw)
    assert(game.position.opponentHasInsufficientMaterial)

  test("Must auto-draw if horde is stalemated and only king can move"):
    val position = FullFen("b7/pk6/P7/P7/8/8/8/8 b - - 0 1")
    val game     = fenToGame(position, Horde)
    assert(game.position.autoDraw)
    assert(game.position.opponentHasInsufficientMaterial)

  test("Must not auto-draw if horde is not stalemated after the only king move"):
    val position = FullFen("8/1b5r/1P6/1Pk3q1/1PP5/r1P5/P1P5/2P5 b - - 0 52")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.autoDraw)
    assertNot(game.position.opponentHasInsufficientMaterial)

  test("Must not auto-draw if not all black King moves leads to stalemate"):
    val position = FullFen("8/8/8/7k/7P/7P/8/8 b - - 0 58")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.autoDraw)
    assertNot(game.position.end)
    assertEquals(game.position.status, None)
    assertNot(Horde.isInsufficientMaterial(game.position))
    assertNot(game.position.opponentHasInsufficientMaterial)

  test("Must not auto-draw in B vs K endgame, king can win"):
    val position = FullFen("7B/6k1/8/8/8/8/8/8 b - -")
    val game     = fenToGame(position, Horde)
    assertNot(game.position.autoDraw)
    assert(game.position.opponentHasInsufficientMaterial)

  test("Pawn on first rank should able to move two squares"):
    val position = FullFen("8/pp1k2q1/3P2p1/8/P3PP2/PPP2r2/PPP5/PPPP4 w - - 1 2")
    val game     = fenToGame(position, Horde)
    assert(game.position.legalMoves.exists(m => m.orig == Square.D1 && m.dest == Square.D3))

  test("Cannot en passant a pawn from the first rank"):
    val position = FullFen("k7/5p2/4p2P/3p2P1/2p2P2/1p2P2P/p2P2P1/2P2P2 w - - 0 1")
    val game     = fenToGame(position, Horde)(Square.C1, Square.C3).get
    assertNot(game._1.position.legalMoves.exists(m => m.orig == Square.B3 && m.dest == Square.C2))

  test("Castle with one rook moved"):
    val sans = SanStr.from(
      "a5 h5 a4 Nc6 a3 b6 a2 Bb7 d5 d6 d4 Rh6 cxd6 Qxd6 f6"
        .split(' ')
        .toVector
    )
    assertRight(Horde.initialPosition.playMoves(sans)): steps =>
      assertEquals(steps.last.after.legalMoves.exists(_.castles), true)

  test("UnmovedRooks & castles at the starting position"):
    val board = Horde.initialPosition
    assertEquals(board.history.unmovedRooks, UnmovedRooks(Set(Square.A8, Square.H8)))
    assertEquals(board.history.castles, Castles(false, false, true, true))

  test("the h8 rooks move"):
    val position = FullFen("r3kbnr/p1pqppp1/1pnp3P/PPPP1P1P/PPP1PPP1/1PPP1PPP/PPPPPPPP/PPPPPPPP b kq - 0 7")
    val game     = fenToGame(position, Horde)(Square.H8, Square.H6).get
    assertEquals(game._1.position.history.unmovedRooks, UnmovedRooks(Set(Square.A8)))
    assertEquals(game._1.position.history.castles, Castles(false, false, false, true))
