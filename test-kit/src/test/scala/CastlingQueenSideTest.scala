package chess

import scala.language.implicitConversions

import Square.*
import variant.Chess960
import format.FullFen

class CastlingQueenSideTest extends ChessTest:

  import compare.dests

  val goodHist = """
PPPPPPPP
R   KB R"""
  val badHist  = goodHist.updateHistory(_.withoutCastles(White))
  test("impossible: near queen in the way"):
    assertEquals(goodHist.place(White.queen, D1).flatMap(_.destsFrom(E1)), Set())
  test("impossible: bishop in the way"):
    assertEquals(goodHist.place(White.bishop, C1).flatMap(_.destsFrom(E1)), Set(D1))
  test("impossible: distant knight in the way"):
    assertEquals(goodHist.place(White.knight, C1).flatMap(_.destsFrom(E1)), Set(D1))
  test("impossible: not allowed by history"):
    assertEquals(badHist.destsFrom(E1), Set(D1))
  test("possible: viable moves"):
    val game = Game(goodHist, White)
    assertEquals(game.board.destsFrom(E1), Set(A1, C1, D1))
  test("possible: correct new board"):
    val game = Game(goodHist, White)
    assertGame(
      game.playMove(E1, C1).get,
      """
PPPPPPPP
  KR B R"""
    )

  val board = """
PPPPPPPP
R   K  R""".updateHistory(_ => castleHistory(White, kingSide = true, queenSide = true))
  test("impact history: if king castles kingside"):
    val game = Game(board, White)
    val g2   = game.playMove(E1, G1).get
    assertGame(
      g2,
      """
PPPPPPPP
R    RK """
    )
    assertEquals(g2.board.destsFrom(G1), Set(H1))
    assertEquals(
      g2.board
        .seq(
          _.move(F1, H1),
          _.move(G1, E1)
        )
        .flatMap(_.destsFrom(E1)),
      Set(D1, F1)
    )
  test("impact history: if king castles queenside"):
    val game = Game(board, White)
    val g2   = game.playMove(E1, C1).get
    assertGame(
      g2,
      """
PPPPPPPP
  KR   R"""
    )
    assertEquals(g2.board.destsFrom(C1), Set(B1))
    assertEquals(
      g2.board
        .seq(
          _.move(D1, A1),
          _.move(C1, E1)
        )
        .flatMap(_.destsFrom(E1)),
      Set(D1, F1)
    )
  test("impact history: if king moves to the right"):
    val game = Game(board, White)
    val g2   = game.playMove(E1, F1).get.as(White)
    assertEquals(g2.board.destsFrom(F1), Set(E1, G1))
    val g3 = g2.playMove(F1, E1).get.as(White)
    assertEquals(g3.board.destsFrom(E1), Set(D1, F1))
  test("impact history: if king moves to the left"):
    val game = Game(board, White)
    val g2   = game.playMove(E1, D1).get.as(White)
    assertEquals(g2.board.destsFrom(D1), Set(C1, E1))
    val g3 = g2.playMove(D1, E1).get.as(White)
    assertEquals(g3.board.destsFrom(E1), Set(D1, F1))
  test("impact history: if kingside rook moves"):
    val game = Game(board, White)
    val g2   = game.playMove(H1, G1).get.as(White)
    assertEquals(g2.board.destsFrom(E1), Set(C1, D1, F1, A1))
    val g3 = g2.playMove(A1, B1).get
    assertEquals(g3.board.destsFrom(E1), Set(D1, F1))
  test("impact history: if queenside rook moves"):
    val game = Game(board, White)
    val g2   = game.playMove(A1, B1).get.as(White)
    assertEquals(g2.board.destsFrom(E1), Set(D1, F1, G1, H1))
    val g3 = g2.playMove(H1, G1).get
    assertEquals(g3.board.destsFrom(E1), Set(D1, F1))

  test("chess960"):
    val fenPosition = FullFen("r3k2r/8/8/8/8/8/8/1R2K2R b KQk - 1 1")
    val init        = fenToGame(fenPosition, Chess960)
    val game        = init.playMoves((A8, A1), (H1, H2), (A1, A7)).get
    assert(game.situation.legalMoves.exists(_.castles))

  test("test games"):
    val fenPosition = FullFen("1rnk1bqr/pppp1bpp/1n2p3/5p2/5P2/1N1N4/PPPPPBPP/1R1K1BQR w KQkq - 0 5")
    val init        = fenToGame(fenPosition, Chess960)
    assert(init.situation.legalMoves.exists(_.castles))
    assert(init.situation.legalMoves.exists(_.castle.exists(_.side == QueenSide)))
