package chess

import chess.format.Visual.addNewLines
import chess.Square.*
import chess.format.{ EpdFen, Fen }
import chess.variant.Standard

class PlayTest extends ChessTest:

  test("only kingside rights"):
    val game = fenToGame(
      EpdFen("4k2r/8/8/6R1/6r1/3K4/8/8 b k - 3 4"),
      Standard
    ).playMoves(
      G4 -> G2,
      G5 -> G8,
      G2 -> G8,
      D3 -> E3,
      G8 -> G5
    )
    assertRight(game): game =>
      val fen = Fen.write(game)
      fen == EpdFen("4k2r/8/8/6r1/8/4K3/8/8 w k - 2 3")

  test("kingside and queenside rights"):
    val game = fenToGame(
      EpdFen("r3k2r/8/8/6R1/6r1/3K4/8/8 b kq - 3 4"),
      Standard
    ).playMoves(
      G4 -> G2,
      G5 -> G8,
      G2 -> G8,
      D3 -> E3,
      G8 -> G5
    )
    assertRight(game): game =>
      val fen = Fen.write(game)
      fen == EpdFen("r3k2r/8/8/6r1/8/4K3/8/8 w kq - 2 3")

  val game =
    makeGame.playMoves(E2 -> E4, E7 -> E5, F1 -> C4, G8 -> F6, D2 -> D3, C7 -> C6, C1 -> G5, H7 -> H6).get
  test("current game"):
    assertEquals(
      addNewLines(game.board.visual),
      """
rnbqkb r
pp p pp
  p  n p
    p B
  B P
   P
PPP  PPP
RN QK NR
"""
    )
  test("after recapture"):
    assertEquals(
      addNewLines(game.playMoves(G5 -> F6, D8 -> F6).get.board.visual),
      """
rnb kb r
pp p pp
  p  q p
    p
  B P
   P
PPP  PPP
RN QK NR
"""
    )
  test("Deep Blue vs Kasparov 1"):
    val g = makeGame
      .playMoves(
        E2 -> E4,
        C7 -> C5,
        C2 -> C3,
        D7 -> D5,
        E4 -> D5,
        D8 -> D5,
        D2 -> D4,
        G8 -> F6,
        G1 -> F3,
        C8 -> G4,
        F1 -> E2,
        E7 -> E6,
        H2 -> H3,
        G4 -> H5,
        E1 -> G1,
        B8 -> C6,
        C1 -> E3,
        C5 -> D4,
        C3 -> D4,
        F8 -> B4
      )
      .get
    assertEquals(
      addNewLines(g.board.visual),
      """
r   k  r
pp   ppp
  n pn
   q   b
 b P
    BN P
PP  BPP
RN Q RK
"""
    )

  test("Peruvian Immortal"):
    val g = makeGame
      .playMoves(
        E2 -> E4,
        D7 -> D5,
        E4 -> D5,
        D8 -> D5,
        B1 -> C3,
        D5 -> A5,
        D2 -> D4,
        C7 -> C6,
        G1 -> F3,
        C8 -> G4,
        C1 -> F4,
        E7 -> E6,
        H2 -> H3,
        G4 -> F3,
        D1 -> F3,
        F8 -> B4,
        F1 -> E2,
        B8 -> D7,
        A2 -> A3,
        E8 -> C8,
        A3 -> B4,
        A5 -> A1,
        E1 -> D2,
        A1 -> H1,
        F3 -> C6,
        B7 -> C6,
        E2 -> A6
      )
      .get
    assertEquals(
      addNewLines(g.board.visual),
      """
  kr  nr
p  n ppp
B p p

 P P B
  N    P
 PPK PP
       q
"""
    )
