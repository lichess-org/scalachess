package chess

import scala.language.implicitConversions
import Pos.*
import bitboard.Bitboard.*
import chess.bitboard.Bitboard

class BishopTest extends ChessTest:

  "a bishop" should {

    val bishop = White - Bishop

    "move in any of 8 positions, 2 and 1 squares away" in {
      pieceMoves(bishop, E4) must bePoss(F3, G2, H1, D5, C6, B7, A8, D3, C2, B1, F5, G6, H7)
    }

    "move in any of 8 positions, 2 and 1 squares away, even when at the edges" in {
      pieceMoves(bishop, H7) must bePoss(G8, G6, F5, E4, D3, C2, B1)
    }

    "not move to positions that are occupied by the same colour" in {
      val board = """
k B



N B    P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
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
    }

    "capture opponent pieces" in {
      val board = """
k B
     q
p

N B    P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
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
    }
  }
