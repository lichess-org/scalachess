package chess

import scala.language.implicitConversions

import Square.*

class KingTest extends ChessTest:

  import compare.dests

  val king = White - King

  test("move 1 position in any direction"):
    assertEquals(pieceMoves(king, D4), Set(D3, C3, C4, C5, D5, E5, E4, E3))

  test("move 1 position in any direction, even from the edges"):
    assertEquals(pieceMoves(king, H8), Set(H7, G7, G8))

  test("move behind pawn barrier"):
    assertEquals(
      """
PPPPPPPP
R  QK NR""".destsFrom(E1),
      Set(F1)
    )

  test("not move to positions that are occupied by the same colour"):
    val board = """
   P
NPKP   P

PPPPPPPP
 NBQQBNN
"""
    assertEquals(
      visualDests(board, board.destsFrom(C4)),
      """



 xxP
NPKP   P
 xxx
PPPPPPPP
 NBQQBNN
"""
    )

  test("capture hanging opponent pieces"):
    val board = """
 bpp   k
  Kp
 p

"""
    assertEquals(
      visualDests(board, board.destsFrom(C3)),
      """




 xxx   k
  Kp
 x

"""
    )
  test("not move near from the other king"):
    assertEquals(
      """
   k
 K
""".destsFrom(B1),
      Set(A1, A2, B2)
    )
