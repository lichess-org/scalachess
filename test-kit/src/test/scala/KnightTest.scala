package chess

import scala.language.implicitConversions
import Square.*

class KnightTest extends ChessTest:

  test("not move to positions that are occupied by the same colour"):
    val board = """
k B

   B
    P
  N
    P
PPP  PPP
 NBQKBNR
"""
    assertEquals(
      visualDests(board, board `destsFrom` C4),
      """
k B

 x B
x   P
  N
x   P
PPPx PPP
 NBQKBNR
"""
    )

  test("capture opponent pieces"):
    val board = """
k B

 b B
n
  N
    b
PPP  PPP
 NBQKBNR
"""
    assertEquals(
      visualDests(board, board `destsFrom` C4),
      """
k B

 x B
x   x
  N
x   x
PPPx PPP
 NBQKBNR
"""
    )
