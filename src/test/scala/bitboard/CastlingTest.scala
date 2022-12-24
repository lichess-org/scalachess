package chess
package bitboard

import munit.FunSuite

class CastlingTest extends FunSuite:

  import Bitboard.*
  import Castles.*

  test("corner case") {
    val castle = Bitboard.corners
    assertEquals(castle.whiteKingSide, true)
    assertEquals(castle.whiteQueenSide, true)
    assertEquals(castle.blackKingSide, true)
    assertEquals(castle.blackQueenSide, true)
  }

  test("only white can castle") {
    val castle = 0x81L.bb
    assertEquals(castle.whiteKingSide, true)
    assertEquals(castle.whiteQueenSide, true)
    assertEquals(castle.blackKingSide, false)
    assertEquals(castle.blackQueenSide, false)
  }

  test("only black can castle") {
    val castle = 0x8100000000000000L.bb
    assertEquals(castle.whiteKingSide, false)
    assertEquals(castle.whiteQueenSide, false)
    assertEquals(castle.blackKingSide, true)
    assertEquals(castle.blackQueenSide, true)
  }
