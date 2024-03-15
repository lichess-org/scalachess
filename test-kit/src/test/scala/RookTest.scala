package chess

import scala.language.implicitConversions
import Square.*

class RookTest extends ChessTest:

  import compare.dests

  test("not move to positions that are occupied by the same colour"):
    assertEquals(
      """
k B



N R    P

PPPPPPPP
 NBQKBNR
""" `destsFrom` C4,
      Set(C3, C5, C6, C7, B4, D4, E4, F4, G4)
    )

  test("capture opponent pieces"):
    assertEquals(
      """
k
  b


n R   p

PPPPPPPP
 NBQKBNR
""" `destsFrom` C4,
      Set(C3, C5, C6, C7, B4, A4, D4, E4, F4, G4)
    )
