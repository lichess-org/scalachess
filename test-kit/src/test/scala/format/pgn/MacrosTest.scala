package chess
package format.pgn

class MacrosTest extends munit.FunSuite:

  import macros.*
  test("pgn macro"):
    val pgn = pgn"1. e4 e5 2. Nf3 Nc6"
    assert(pgn.tree.isDefined)
    assertEquals(pgn.toPgn.toString, "1. e4 e5 2. Nf3 Nc6")
