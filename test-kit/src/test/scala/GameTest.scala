package chess

import scala.language.implicitConversions

import Square.*

class GameTest extends ChessTest:

  val game = Game(
    """
    k
 b
R   K""".withColor(color = Black)
  )

  test("prevent castle by capturing a rook: can castle queenside"):
    assert(game.position.history.canCastle(White, QueenSide))
  test("prevent castle by capturing a rook: can still castle queenside"):
    assert(game.playMoves(B2 -> A3).get.position.history.canCastle(White, QueenSide))
  test("prevent castle by capturing a rook: can not castle queenside anymore"):
    assertNot(game.playMoves(B2 -> A1).get.position.history.canCastle(White, QueenSide), false)

  test("update half move clock: start at 0"):
    assertEquals(Game(variant.Standard).halfMoveClock, 0)
  test("update half move clock: increment"):
    Game(variant.Standard)(G1, F3).assertRight: (game, _) =>
      assertEquals(game.halfMoveClock, 1)
  test("update half move clock: not increment"):
    Game(variant.Standard)(E2, E4).assertRight: (game, _) =>
      assertEquals(game.halfMoveClock, 0)
