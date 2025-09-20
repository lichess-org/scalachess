package chess
package format.pgn

class RoundtripTest extends ChessTest:

  test("roundtrip with special chars for tags"):
    val value = "aä\\\"'$%/°á \t\b \"\\\\/"
    val parsed = Parser
      .full(Pgn(tags = Tags(List(Tag(_.Site, value))), InitialComments.empty, None, Ply.initial).render)
      .toOption
      .get
    assertEquals(parsed.tags("Site"), Some(value))
