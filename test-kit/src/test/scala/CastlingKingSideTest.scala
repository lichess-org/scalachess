package chess

import scala.language.implicitConversions

import Square.*
import variant.FromPosition

class CastlingKingSideTest extends ChessTest:

  import compare.dests

  val goodHist = """
PPPPPPPP
R  QK  R"""
  val badHist  = goodHist.updateHistory(_.withoutCastles(White))
  test("impossible"):
    assertEquals(goodHist.place(White.bishop, F1).flatMap(_.destsFrom(E1)), Set())
    assertEquals(goodHist.place(White.knight, G1).flatMap(_.destsFrom(E1)), Set(F1))
    assertEquals(badHist.destsFrom(E1), Set(F1))
    val board960 = """
PPPPPPPP
RQK   R """.chess960.updateHistory(_ => castleHistory(White, kingSide = true, queenSide = true))
    assertEquals(board960.place(White.bishop, D1).flatMap(_.destsFrom(C1)), Set())
    assertEquals(board960.place(White.knight, F1).flatMap(_.destsFrom(C1)), Set(D1))
  test("possible"):
    val game = Game(goodHist, White)
    assertEquals(game.board.destsFrom(E1), Set(F1, G1, H1))
    assertGame(
      game.playMove(E1, G1).get,
      """
PPPPPPPP
R  Q RK """
    )
    val board: Board = """
   PPPPP
B     KR""".chess960
    val g2 = Game(board, White)
    assertEquals(board.destsFrom(G1), Set(F1, H1))
    assertGame(
      g2.playMove(G1, H1).get,
      """
   PPPPP
B    RK """
    )
  test("chess960 close kingside with 2 rooks around"):
    val board: Board = """
PPPPPPPP
RKRBB   """.chess960
    assertEquals(board.destsFrom(B1), Set())
  test("chess960 close queenside"):
    val board: Board = """
PPPPPPPP
RK     B""".chess960
    val game = Game(board, White)
    assertEquals(board.destsFrom(B1), Set(A1, C1))
    assertGame(
      game.playMove(B1, A1).get,
      """
PPPPPPPP
  KR   B"""
    )
  test("chess960 close queenside as black"):
    val game = Game(
      """
 b rkr q
p pppppp
 p n




 K""".chess960,
      Black
    )
    assertEquals(game.board.destsFrom(E8), Set(D8, F8))
    assertGame(
      game.playMove(E8, D8).get,
      """
 bkr r q
p pppppp
 p n




 K"""
    )
  test("from position with chess960 castling"):
    val game = Game(
      makeBoard(
        """rk  r
pppbnppp
  p  n
P  Pp
  P  q
R     NP
PP  PP
KNQRB""",
        FromPosition
      ),
      Black
    )
    assertEquals(game.board.destsFrom(B8), Set(A8, C8, E8))
