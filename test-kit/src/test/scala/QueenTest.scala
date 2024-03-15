package chess

import scala.language.implicitConversions
import Square.*

class QueenTest extends ChessTest:

  test("not move to positions that are occupied by the same colour"):
    val board = """
k B



N Q    P

PPPPPPPP
 NBQKBNR
"""
    assertEquals(
      visualDests(board, board `destsFrom` C4),
      """
k B   x
  x  x
x x x
 xxx
NxQxxxxP
 xxx
PPPPPPPP
 NBQKBNR
"""
    )

  test("capture opponent pieces"):
    val board = """
k B
     q
p

N QP   P

PPPPPPPP
 NBQKBNR
"""
    assertEquals(
      visualDests(board, board `destsFrom` C4),
      """
k B
  x  x
x x x
 xxx
NxQP   P
 xxx
PPPPPPPP
 NBQKBNR
"""
    )
