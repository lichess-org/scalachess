package chess

import scala.language.implicitConversions

import Square.*

class BoardTest extends ChessTest:

  val board = makeBoard

  test("position pieces correctly"):
    assertEquals(
      board.pieces,
      Map(
        A1 -> (White - Rook),
        B1 -> (White - Knight),
        C1 -> (White - Bishop),
        D1 -> (White - Queen),
        E1 -> (White - King),
        F1 -> (White - Bishop),
        G1 -> (White - Knight),
        H1 -> (White - Rook),
        A2 -> (White - Pawn),
        B2 -> (White - Pawn),
        C2 -> (White - Pawn),
        D2 -> (White - Pawn),
        E2 -> (White - Pawn),
        F2 -> (White - Pawn),
        G2 -> (White - Pawn),
        H2 -> (White - Pawn),
        A7 -> (Black - Pawn),
        B7 -> (Black - Pawn),
        C7 -> (Black - Pawn),
        D7 -> (Black - Pawn),
        E7 -> (Black - Pawn),
        F7 -> (Black - Pawn),
        G7 -> (Black - Pawn),
        H7 -> (Black - Pawn),
        A8 -> (Black - Rook),
        B8 -> (Black - Knight),
        C8 -> (Black - Bishop),
        D8 -> (Black - Queen),
        E8 -> (Black - King),
        F8 -> (Black - Bishop),
        G8 -> (Black - Knight),
        H8 -> (Black - Rook)
      )
    )

  test("have pieces by default"):
    assertNot(board.allPieces.isEmpty)

  test("have castling rights by default"):
    assertEquals(board.history.castles, Castles.init)

  test("allow a piece to be placed"):
    assertEquals(board.place(White - Rook, E3).get.pieceAt(E3), Option(White - Rook))

  test("allow a piece to be taken"):
    board
      .take(A1)
      .assertSome: b =>
        assertEquals(b.pieceAt(A1), None)

  test("allow a piece to move"):
    board
      .move(E2, E4)
      .assertSome: b =>
        assertEquals(b.pieceAt(E4), Option(White - Pawn))

  test("not allow an empty position to move"):
    assertEquals(board.move(E5, E6), None)

  test("not allow a piece to move to an occupied position"):
    assertEquals(board.move(A1, A2), None)

  test("allow chaining actions"):
    makeEmptyBoard
      .seq(
        _.place(White - Pawn, A2),
        _.place(White - Pawn, A3),
        _.move(A2, A4)
      )
      .assertSome: b =>
        assertEquals(b.pieceAt(A4), Option(White - Pawn))

  test("fail on bad actions chain"):
    assertEquals(
      makeEmptyBoard.seq(
        _.place(White - Pawn, A2),
        _.place(White - Pawn, A3),
        _.move(B2, B4)
      ),
      None
    )

  test("detect check: by rook"):
    assert:
      """
  K  r
  """.as(White).check.yes
  test("detect check: by knight"):
    assert:
      ("""
  n
K
""".as(White)).check.yes
  test("detect check: by bishop"):
    assert:
      ("""
  b


     K
""".as(White)).check.yes
  test("detect check: by pawn"):
    assert:
      ("""
    p
     K
""".as(White)).check.yes
  test("detect check: not"):
    assert:
      ("""
   n
K
""".as(White)).check.no
  test("detect check mate: by rook"):
    assert:
      ("""
PP
K  r
""".as(White)).checkMate
  test("detect check mate: by knight"):
    assert:
      ("""
PPn
KR
""".as(White)).checkMate
  test("detect check mate: not"):
    assertNot:
      ("""
  n
K
""".as(White)).checkMate
  test("stale mate: stuck in a corner"):
    assert:
      ("""
prr
K
""".as(White)).staleMate
  test("stale mate: not"):
    assertNot:
      ("""
  b
K
""".as(White)).staleMate

  test("Give the correct winner for a game"):
    val game =
      """
PP
K  r
""".as(White)
    assert(game.checkMate)
    assertEquals(game.winner, Some(Black))

  test("Not give a winner if the game is still in progress"):
    val game = """
      p
      K
      """.as(White)
    assertEquals(game.winner, None)

  test("not be playable: with touching kings"):
    val game = "kK BN".as(Black)
    assertNot(game.playable(true))
    assertNot(game.playable(false))

  test("not be playable: with other side in check"):
    val game = "k Q K".as(White)
    assertNot(game.playable(true))
    assertNot(game.playable(false))
