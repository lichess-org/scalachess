package chess

import scala.language.implicitConversions
import Pos.*

class QueenTest extends ChessTest:

  "a queen" should:

    val queen = White - Queen

    "not move to positions that are occupied by the same colour" in:
      val board = """
k B



N Q    P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
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

    "capture opponent pieces" in:
      val board = """
k B
     q
p

N QP   P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
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
