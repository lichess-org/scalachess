package chess

import macros.*

class MacrosTest extends munit.FunSuite:

  test("pgn macro"):
    val pgn = pgn"1. e4 e5 2. Nf3 Nc6"
    assert(pgn.tree.isDefined)
    assertEquals(pgn.toPgn.toString, "1. e4 e5 2. Nf3 Nc6")

  test("uci macro"):
    val uci = uci"d2d4"
    assert(uci.isInstanceOf[chess.format.Uci.Move])
    assertEquals(uci.uci, "d2d4")
