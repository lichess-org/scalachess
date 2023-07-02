package chess
package format.pgn

import scala.language.implicitConversions

class RoundtripTest extends munit.FunSuite:

  test("roundtrip with special chars for tags"):
    val value = "aä\\\"'$%/°á \t\b \"\\\\/"
    val parsed = Parser
      .full(Pgn(tags = Tags(List(Tag(_.Site, value))), InitialComments.empty, None).render)
      .toOption
      .get
    assertEquals(parsed.tags("Site"), Some(value))
