package chess

import Square.*
import format.Uci

class PawnTest extends ChessTest:

  import compare.dests

  test("move towards rank by 1 square"):
    assertEquals(
      makeBoard(
        A4 -> White.pawn
      ).destsFrom(A4),
      Set(A5)
    )

  test("not move to positions that are occupied by the same color"):
    assertEquals(
      makeBoard(
        A4 -> White.pawn,
        A5 -> White.pawn
      ).destsFrom(A4),
      Set()
    )

  test("capture in diagonal"):
    assertEquals(
      makeBoard(
        D4 -> White.pawn,
        C5 -> Black.pawn,
        E5 -> Black.bishop
      ).destsFrom(D4),
      Set(C5, D5, E5)
    )

  test("require a capture to move in diagonal"):
    assertEquals(
      makeBoard(
        A4 -> White.pawn,
        C5 -> White.pawn
      ).destsFrom(A4),
      Set(A5)
    )

  test("move towards rank by 2 squares"):
    // "if the path is free" in:
    assertEquals(
      makeBoard(
        A2 -> White.pawn
      ).destsFrom(A2),
      Set(A3, A4)
    )
    // "if the path is occupied by a friend" in:
    // "close" in:
    assertEquals(
      makeBoard(
        A2 -> White.pawn,
        A3 -> White.rook
      ).destsFrom(A2),
      Set()
    )
    // "far" in:
    assertEquals(
      makeBoard(
        A2 -> White.pawn,
        A4 -> White.rook
      ).destsFrom(A2),
      Set(A3)
    )
    // "if the path is occupied by a enemy" in:
    // "close" in:
    assertEquals(
      makeBoard(
        A2 -> White.pawn,
        A3 -> Black.rook
      ).destsFrom(A2),
      Set()
    )
    // "far" in:
    assertEquals(
      makeBoard(
        A2 -> White.pawn,
        A4 -> Black.rook
      ).destsFrom(A2),
      Set(A3)
    )
  test("capture en passant"):
    // "with proper position" in:
    val board = makeBoard(
      D5 -> White.pawn,
      C5 -> Black.pawn,
      E5 -> Black.pawn
    )
    // "without history" in:
    assertEquals(board.destsFrom(D5), Set(D6))
    // "with irrelevant history" in:
    assertEquals(
      board
        .withHistory(
          defaultHistory(
            lastMove = Option(Uci.Move(A2, A4))
          )
        )
        .destsFrom(D5),
      Set(D6)
    )
    // "with relevant history on the left" in:
    assertEquals(
      board
        .withHistory(
          defaultHistory(
            lastMove = Option(Uci.Move(C7, C5))
          )
        )
        .destsFrom(D5),
      Set(D6, C6)
    )
    // "with relevant history on the right" in:
    assertEquals(
      board
        .withHistory(
          defaultHistory(
            lastMove = Option(Uci.Move(E7, E5))
          )
        )
        .destsFrom(D5),
      Set(D6, E6)
    )
    // "enemy not-a-pawn" in:
    assertEquals(
      makeBoard(
        D5 -> White.pawn,
        E5 -> Black.rook
      ).withHistory(
        defaultHistory(
          lastMove = Option(Uci.Move(E7, E5))
        )
      ).destsFrom(D5),
      Set(D6)
    )
    // "friend pawn (?!)" in:
    assertEquals(
      makeBoard(
        D5 -> White.pawn,
        E5 -> White.pawn
      ).withHistory(
        defaultHistory(
          lastMove = Option(Uci.Move(E7, E5))
        )
      ).destsFrom(D5),
      Set(D6)
    )

  test("move towards rank by 1 square"):
    assertEquals(
      makeBoard(
        A4 -> Black.pawn
      ).destsFrom(A4),
      Set(A3)
    )

  test("not move to positions that are occupied by the same color"):
    assertEquals(
      makeBoard(
        A4 -> Black.pawn,
        A3 -> Black.pawn
      ).destsFrom(A4),
      Set()
    )

  test("capture in diagonal"):
    assertEquals(
      makeBoard(
        D4 -> Black.pawn,
        C3 -> White.pawn,
        E3 -> White.bishop
      ).destsFrom(D4),
      Set(C3, D3, E3)
    )

  test("require a capture to move in diagonal"):
    assertEquals(
      makeBoard(
        A4 -> Black.pawn,
        C3 -> Black.pawn
      ).destsFrom(A4),
      Set(A3)
    )

  test("move towards rank by 2 squares"):
    // "if the path is free" in:
    assertEquals(
      makeBoard(
        A7 -> Black.pawn
      ).destsFrom(A7),
      Set(A6, A5)
    )
    // "if the path is occupied by a friend" in:
    // "close" in:
    assertEquals(
      makeBoard(
        A7 -> Black.pawn,
        A6 -> Black.rook
      ).destsFrom(A7),
      Set()
    )
    // "far" in:
    assertEquals(
      makeBoard(
        A7 -> Black.pawn,
        A5 -> Black.rook
      ).destsFrom(A7),
      Set(A6)
    )
    // "if the path is occupied by a enemy" in:
    // "close" in:
    assertEquals(
      makeBoard(
        A7 -> Black.pawn,
        A6 -> White.rook
      ).destsFrom(A7),
      Set()
    )
    // "far" in:
    assertEquals(
      makeBoard(
        A7 -> Black.pawn,
        A5 -> White.rook
      ).destsFrom(A7),
      Set(A6)
    )
  test("capture en passant"):
    // "with proper position" in:
    val board = makeBoard(
      D4 -> Black.pawn,
      C4 -> White.pawn,
      E4 -> White.pawn
    )
    // "without history" in:
    assertEquals(board.destsFrom(D4), Set(D3))
    // "with relevant history on the left" in:
    assertEquals(
      board
        .withHistory(
          defaultHistory(
            lastMove = Option(Uci.Move(C2, C4))
          )
        )
        .destsFrom(D4),
      Set(D3, C3)
    )
    // "enemy not-a-pawn" in:
    assertEquals(
      makeBoard(
        D4 -> Black.pawn,
        E4 -> White.rook
      ).withHistory(
        defaultHistory(
          lastMove = Option(Uci.Move(E2, E4))
        )
      ).destsFrom(D4),
      Set(D3)
    )
