package chess

import Pos._

class DoomTest extends ChessTest {

  "a duke" should {

    val duke = White - Duke

    "move in any direction until range 2" in {
      pieceMoves(duke, D4) must bePoss(
        D5,
        D6,
        D3,
        D2,
        E4,
        F4,
        C4,
        B4,
        C3,
        B2,
        E5,
        F6,
        C5,
        B6,
        E3,
        F2
      )
    }

    "move 1 position in any direction, even from the edges" in {
      pieceMoves(duke, H8) must bePoss(
        F8,
        G8,
        F6,
        G7,
        H7,
        H6
      )
    }

    "not move to positions that are occupied by the same colour" in {
      val board = """
k B



N D    P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
        """
k B

x x x
 xxx
NxDxx  P
 xxx
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

N DP   P

PPPPPPPP
 NBQKBNR
"""
      board destsFrom C4 must bePoss(
        board,
        """
k B

x x x
 xxx
NxDPX  P
 xxx
PPPPPPPP
 NBQKBNR
"""
      )
    }
    "threaten" in {
      val board = """
k B
  q  q
p

n D   Pp

PPPPPPPP
 NBQKBNR
"""
      "a reachable enemy - horizontal" in {
        board actorAt C4 map (_ threatens A4) must beSome(true)
      }
      "a reachable enemy - diagonal" in {
        board actorAt C4 map (_ threatens A6) must beSome(true)
      }
      "an unreachable enemy" in {
        board actorAt C4 map (_ threatens H4) must beSome(false)
      }
      "a reachable friend" in {
        board actorAt C4 map (_ threatens C2) must beSome(true)
      }
      "nothing reachable" in {
        board actorAt C4 map (_ threatens B5) must beSome(true)
      }
      "nothing unreachable" in {
        board actorAt C4 map (_ threatens B6) must beSome(false)
      }
    }
  }
}
