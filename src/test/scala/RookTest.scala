package chess

import scala.language.implicitConversions
import Square.*

class RookTest extends ChessTest:

  "a rook" should:

    "not move to positions that are occupied by the same colour" in:
      """
k B



N R    P

PPPPPPPP
 NBQKBNR
""" destsFrom C4 must bePoss(C3, C5, C6, C7, B4, D4, E4, F4, G4)

    "capture opponent pieces" in:
      """
k
  b


n R   p

PPPPPPPP
 NBQKBNR
""" destsFrom C4 must bePoss(C3, C5, C6, C7, B4, A4, D4, E4, F4, G4)
