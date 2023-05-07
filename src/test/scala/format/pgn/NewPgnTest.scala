package chess
package format.pgn

import scala.language.implicitConversions

import cats.syntax.all.*
import munit.FunSuite

class PgnTest extends FunSuite:

  given Conversion[String, SanStr]  = SanStr(_)
  given Conversion[String, Comment] = Comment(_)

  val pgn1 = TestPgn(
    tags = Tags(
      List(
        Tag(_.White, "Kramnik,V"),
        Tag(_.Black, "Anand,V"),
        Tag(_.ECO, "D14")
      )
    ),
    turns = List(
      Turn(
        number = 1,
        white = TestMove("d4").some,
        black = TestMove("d5").some
      ),
      Turn(
        number = 2,
        white = TestMove("c4", glyphs = glyphs(1)).some,
        black = TestMove("c6", glyphs = glyphs(2)).some
      ),
      Turn(
        number = 3,
        white = TestMove("Nc3", glyphs = glyphs(3)).some,
        black = TestMove("Nf6").some
      ),
      Turn(
        number = 4,
        white = TestMove(
          "cxd5",
          comments =
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one" :: Nil
        ).some,
        black = TestMove("cxd5").some
      ),
      Turn(
        number = 5,
        white = TestMove("Bf4").some,
        black = TestMove("Nc6").some
      )
    )
  )

  val pgn2 = TestPgn(
    tags = Tags(
      List(
        Tag(_.White, "tsinnema"),
        Tag(_.Black, "stockfish"),
        Tag(_.TimeControl, "300"),
        Tag(_.ECO, "A00e")
      )
    ),
    turns = List(
      Turn(
        number = 1,
        white = TestMove("a4", secondsLeft = 298.some).some,
        black = TestMove("Nf6", secondsLeft = 299.some).some
      ),
      Turn(
        number = 2,
        white = TestMove("d4", secondsLeft = 295.some).some,
        black = TestMove("d5", secondsLeft = 298.some).some
      ),
      Turn(
        number = 3,
        white = TestMove("h4", secondsLeft = 292.some).some,
        black = TestMove("e6", secondsLeft = 297.some).some
      ),
      Turn(
        number = 4,
        white = TestMove(
          "Qd3",
          glyphs = glyphs(1),
          secondsLeft = 288.some,
          comments = "An invention of true genius." :: Nil
        ).some,
        black = TestMove("c5", secondsLeft = 296.some).some
      ),
      Turn(
        number = 5,
        white = TestMove("dxc5", secondsLeft = 258.some).some,
        black = TestMove("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
      )
    )
  )

  val pgn3 = TestPgn(
    tags = Tags.empty,
    turns = List(
      Turn(
        number = 1,
        white = TestMove("d3", glyphs = glyphs(6)).some,
        black = TestMove("Nc6", glyphs = glyphs(10)).some
      ),
      Turn(
        number = 2,
        white = TestMove("Qd2").some,
        black = TestMove(
          "Nb4",
          glyphs = Glyphs(
            Glyph.MoveAssessment.blunder.some,
            Glyph.PositionAssessment.whiteMuchBetter.some,
            List(Glyph.Observation.timeTrouble)
          )
        ).some
      ),
      Turn(
        number = 3,
        white = TestMove("Qxb4", glyphs = glyphs(7)).some,
        black = None
      )
    )
  )

  val pgn4 = TestPgn(
    tags = Tags.empty,
    turns = List(
      Turn(
        number = 1,
        white = TestMove(
          "d4",
          variations = List(
            List(
              Turn(
                number = 1,
                white = TestMove("e4").some,
                black = None
              )
            )
          )
        ).some,
        black = TestMove(
          "Nf6",
          variations = List(
            List(
              Turn(
                number = 1,
                white = None,
                black = TestMove("d5").some
              )
            )
          )
        ).some
      )
    )
  )
  val pgn5 = TestPgn(
    tags = Tags(
      List(
        Tag(_.White, "tsinnema"),
        Tag(_.Black, "[=0040.34h5a4]"),
        Tag(_.TimeControl, "300"),
        Tag(_.ECO, "A00e")
      )
    ),
    turns = List(
      Turn(
        number = 1,
        white = TestMove("a4", secondsLeft = 298.some).some,
        black = TestMove("Nf6", secondsLeft = 299.some).some
      ),
      Turn(
        number = 2,
        white = TestMove("d4", secondsLeft = 295.some).some,
        black = TestMove("d5", secondsLeft = 298.some).some
      ),
      Turn(
        number = 3,
        white = TestMove("h4", secondsLeft = 292.some).some,
        black = TestMove("e6", secondsLeft = 297.some).some
      ),
      Turn(
        number = 4,
        white = TestMove(
          "Qd3",
          glyphs = glyphs(1),
          secondsLeft = 288.some,
          comments = "An invention of true genius." :: Nil
        ).some,
        black = TestMove("c5", secondsLeft = 296.some).some
      ),
      Turn(
        number = 5,
        white = TestMove("dxc5", secondsLeft = 258.some).some,
        black = TestMove("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
      )
    )
  )

  val pgn6 = TestPgn(
    tags = Tags(
      List(
        Tag(_.Result, "0-1")
      )
    ),
    turns = List()
  )

  val pgn7 = TestPgn(
    tags = Tags.empty,
    turns = List()
  )

  val pgn8 = TestPgn(
    tags = Tags.empty,
    turns = List(),
    initial = Initial(List("Why hello there!"))
  )

  val pgn9 = TestPgn(
    tags = Tags.empty,
    turns = List(),
    initial = Initial(
      List(
        "Why hello there!",
        "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
      )
    )
  )

  val pgn10 = TestPgn(
    tags = Tags.empty,
    turns = List(
      Turn(
        number = 1,
        white = TestMove(
          "d4",
          variations = List(
            List(
              Turn(
                number = 1,
                white = TestMove("e4").some,
                black = None
              )
            )
          )
        ).some,
        black = TestMove(
          "Nf6",
          variations = List(
            List(
              Turn(
                number = 1,
                white = None,
                black = TestMove("d5").some
              )
            )
          )
        ).some
      )
    ),
    initial = Initial(
      List(
        "Why hello there!",
        "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
      )
    )
  )

  val pgn11 = TestPgn(
    tags = Tags(
      List(
        Tag(_.TimeControl, "\"")
      )
    ),
    turns = List()
  )

  val pgn12 = TestPgn(
    tags = Tags(
      List(
        Tag(_.TimeControl, "\\")
      )
    ),
    turns = List()
  )

  val pgns = List(pgn1, pgn2, pgn3, pgn4, pgn5, pgn6, pgn7, pgn8, pgn9, pgn10, pgn11, pgn12)

  test("TestPgn and Pgn PgnStr"):
    pgns.foreach { pgn =>
      val newPgn = NewPgn(pgn)
      assertEquals(pgn.toString, newPgn.render.value)
    }
