package chess

import scala.language.implicitConversions
import Square.*

class BishopTest extends ChessTest:

  test("not move to positions that are occupied by the same colour"):
    val board = """
k B



N B    P

PPPPPPPP
 NBQKBNR
"""
    assertEquals(
      visualDests(board, board.destsFrom(C4)),
      """
k B   x
     x
x   x
 x x
N B    P
 x x
PPPPPPPP
 NBQKBNR
"""
    )

  test("capture opponent pieces"):
    val board = """
k B
     q
p

N B    P

PPPPPPPP
 NBQKBNR
"""
    assertEquals(
      visualDests(board, board.destsFrom(C4)),
      """
k B
     x
x   x
 x x
N B    P
 x x
PPPPPPPP
 NBQKBNR
"""
    )
