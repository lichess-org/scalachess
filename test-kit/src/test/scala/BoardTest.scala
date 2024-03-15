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
    assertEquals(board.place(White - Rook, E3).get.apply(E3), Option(White - Rook))

  test("allow a piece to be taken"):
    board `take` A1 `assertSome`: b =>
      assertEquals(b(A1), None)

  test("allow a piece to move"):
    board.move(E2, E4) `assertSome`: b =>
      assertEquals(b(E4), Option(White - Pawn))

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
        assertEquals(b(A4), Option(White - Pawn))

  test("fail on bad actions chain"):
    assertEquals(
      makeEmptyBoard.seq(
        _.place(White - Pawn, A2),
        _.place(White - Pawn, A3),
        _.move(B2, B4)
      ),
      None
    )
