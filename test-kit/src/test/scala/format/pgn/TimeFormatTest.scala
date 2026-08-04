package chess
package format.pgn

import cats.syntax.all.*
import scalalib.model.Seconds
import scalalib.time.*

class TimeFormatTest extends ChessTest:

  test("format seconds"):
    def f(s: Int) =
      val builder = new StringBuilder
      Move.formatPgnSeconds(Seconds(s), builder)
      builder.toString
    assertEquals(f(0), "0:00:00")
    assertEquals(f(9), "0:00:09")
    assertEquals(f(60), "0:01:00")
    assertEquals(f(79835), "22:10:35")
    assertEquals(f(979835), "272:10:35")

  test("clocks pgn"):
    def f(c: Option[Int], e: Option[Int]) = Move.clockString(c.map(Seconds(_)), e.map(Seconds(_)))
    assertEquals(f(79835.some, 1391.some), "[%clk 22:10:35] [%emt 0:23:11]".some)
    assertEquals(f(60.some, none), "[%clk 0:01:00]".some)
    assertEquals(f(none, 11.some), "[%emt 0:00:11]".some)
    assertEquals(f(none, none), none)

  test("format PGN tags"):
    assertEquals(Tag.UTCDate.format.format(millisToDateTime(1680424483730L)), "2023.04.02")
    assertEquals(Tag.UTCTime.format.format(millisToDateTime(1680424483730L)), "08:34:43")
