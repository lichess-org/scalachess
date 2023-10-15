package chess

import scala.language.implicitConversions
import Square.*

class KnightTest extends ChessSpecs:

  "a knight" should:

    "not move to positions that are occupied by the same colour" in:
      val board = """
k B

   B
    P
  N
    P
PPP  PPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
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

    "capture opponent pieces" in:
      val board = """
k B

 b B
n
  N
    b
PPP  PPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
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
