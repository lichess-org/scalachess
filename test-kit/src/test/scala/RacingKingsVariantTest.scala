package chess

import chess.format.FullFen

import variant.RacingKings

class RacingKingsVariantTest extends ChessTest:

  test("disallow discovered check"):
    val fenPosition = FullFen("1r6/8/5qRK/8/7Q/8/2knNn2/2b2B2 b - - 11 11")
    val game        = fenToGame(fenPosition, RacingKings)
    assertEquals(game.position.destinations.get(Square.D2), None)

  test("game end to black"):
    val fenPosition = FullFen("4krn1/K2b4/8/8/8/8/8/8 w - - 4 3")
    val game        = fenToGame(fenPosition, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Option(Black))

  test("game end to black 2"):
    val fenPosition = FullFen("4brk1/8/5n2/K7/8/8/8/8 w - - 6 4")
    val game        = fenToGame(fenPosition, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Option(Black))

  test("game end to black 3"):
    val fenPosition = FullFen("3kbrn1/8/8/K7/8/8/8/8 w - - 4 3")
    val game        = fenToGame(fenPosition, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Option(Black))

  test("game end to black 4"):
    val fenPosition = FullFen("4brk1/4n3/8/K7/8/8/8/8 w - - 4 3")
    val game        = fenToGame(fenPosition, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Option(Black))

  test("game end to white"):
    val fenPosition = FullFen("K3br2/5k2/8/8/6n1/8/8/8 w - - 4 3")
    val game        = fenToGame(fenPosition, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Option(White))

  test("game end to white 2"):
    val fenPosition = FullFen("K3b2r/5k2/5n2/8/8/8/8/8 w - - 4 3")
    val game        = fenToGame(fenPosition, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, Option(White))

  test("game is draw if both kings are in 8th rank"):
    val fenPosition = FullFen("K3brk1/8/5n2/8/8/8/8/8 w - - 4 3")
    val game        = fenToGame(fenPosition, RacingKings)
    assert(game.position.end)
    assertEquals(game.position.winner, None)

  test("game is not end when Black's King can go to the 8th rank"):
    val fenPosition = FullFen("1K2br2/5k2/5n2/8/8/8/8/8 b - - 3 2")
    val game        = fenToGame(fenPosition, RacingKings)
    assertNot(game.position.end)
