package chess

import chess.format.{ FullFen, Uci }

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

  test("Castle lastMove UCI normalization: standard"):
    val from = fenToGame(
      FullFen("rnbqkbnr/ppp2ppp/8/3pp3/4P3/3B1N2/PPPP1PPP/RNBQK2R w KQkq - 0 4"),
      variant.Standard
    )
    assertEquals(from(E1, G1).get._1.history.lastMove, Uci("e1h1"))
    assertEquals(from(E1, H1).get._1.history.lastMove, Uci("e1h1"))

  test("Castle lastMove UCI normalization: chess960"):
    val from = fenToGame(
      FullFen("rnbqkbnr/ppp2ppp/8/3pp3/4P3/3B1N2/PPPP1PPP/RNBQK2R w KQkq - 0 4"),
      variant.Chess960
    )
    assertEquals(from(E1, H1).get._1.history.lastMove, Uci("e1h1"))
