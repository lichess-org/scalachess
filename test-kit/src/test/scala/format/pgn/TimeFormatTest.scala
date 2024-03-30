package chess
package format.pgn

import scalalib.time.*

class TimeFormatTest extends ChessTest:

  test("format seconds"):
    assertEquals(Move.formatPgnSeconds(0), "0:00:00")
    assertEquals(Move.formatPgnSeconds(9), "0:00:09")
    assertEquals(Move.formatPgnSeconds(60), "0:01:00")
    assertEquals(Move.formatPgnSeconds(79835), "22:10:35")
    assertEquals(Move.formatPgnSeconds(979835), "272:10:35")

  test("format PGN tags"):
    assertEquals(Tag.UTCDate.format.format(millisToDateTime(1680424483730L)), "2023.04.02")
    assertEquals(Tag.UTCTime.format.format(millisToDateTime(1680424483730L)), "08:34:43")
