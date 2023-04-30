package chess
package format.pgn

import scala.language.implicitConversions

import cats.syntax.all.*
import munit.FunSuite

class NewPgnTest extends FunSuite:

  given Conversion[String, SanStr]  = SanStr(_)
  given Conversion[String, Comment] = Comment(_)

  val pgn1 = Pgn(
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
        white = Move("d4").some,
        black = Move("d5").some
      ),
      Turn(
        number = 2,
        white = Move("c4", glyphs = glyphs(1)).some,
        black = Move("c6", glyphs = glyphs(2)).some
      ),
      Turn(
        number = 3,
        white = Move("Nc3", glyphs = glyphs(3)).some,
        black = Move("Nf6").some
      ),
      Turn(
        number = 4,
        white = Move(
          "cxd5",
          comments =
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one" :: Nil
        ).some,
        black = Move("cxd5").some
      ),
      Turn(
        number = 5,
        white = Move("Bf4").some,
        black = Move("Nc6").some
      )
    )
  )

  val pgn2 = Pgn(
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
        white = Move("a4", secondsLeft = 298.some).some,
        black = Move("Nf6", secondsLeft = 299.some).some
      ),
      Turn(
        number = 2,
        white = Move("d4", secondsLeft = 295.some).some,
        black = Move("d5", secondsLeft = 298.some).some
      ),
      Turn(
        number = 3,
        white = Move("h4", secondsLeft = 292.some).some,
        black = Move("e6", secondsLeft = 297.some).some
      ),
      Turn(
        number = 4,
        white = Move(
          "Qd3",
          glyphs = glyphs(1),
          secondsLeft = 288.some,
          comments = "An invention of true genius." :: Nil
        ).some,
        black = Move("c5", secondsLeft = 296.some).some
      ),
      Turn(
        number = 5,
        white = Move("dxc5", secondsLeft = 258.some).some,
        black = Move("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
      )
    )
  )

  val pgn3 = Pgn(
    tags = Tags.empty,
    turns = List(
      Turn(
        number = 1,
        white = Move("d3", glyphs = glyphs(6)).some,
        black = Move("Nc6", glyphs = glyphs(10)).some
      ),
      Turn(
        number = 2,
        white = Move("Qd2").some,
        black = Move(
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
        white = Move("Qxb4", glyphs = glyphs(7)).some,
        black = None
      )
    )
  )

  val pgn4 = Pgn(
    tags = Tags.empty,
    turns = List(
      Turn(
        number = 1,
        white = Move(
          "d4",
          variations = List(
            List(
              Turn(
                number = 1,
                white = Move("e4").some,
                black = None
              )
            )
          )
        ).some,
        black = Move(
          "Nf6",
          variations = List(
            List(
              Turn(
                number = 1,
                white = None,
                black = Move("d5").some
              )
            )
          )
        ).some
      )
    )
  )
  val pgn5 = Pgn(
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
        white = Move("a4", secondsLeft = 298.some).some,
        black = Move("Nf6", secondsLeft = 299.some).some
      ),
      Turn(
        number = 2,
        white = Move("d4", secondsLeft = 295.some).some,
        black = Move("d5", secondsLeft = 298.some).some
      ),
      Turn(
        number = 3,
        white = Move("h4", secondsLeft = 292.some).some,
        black = Move("e6", secondsLeft = 297.some).some
      ),
      Turn(
        number = 4,
        white = Move(
          "Qd3",
          glyphs = glyphs(1),
          secondsLeft = 288.some,
          comments = "An invention of true genius." :: Nil
        ).some,
        black = Move("c5", secondsLeft = 296.some).some
      ),
      Turn(
        number = 5,
        white = Move("dxc5", secondsLeft = 258.some).some,
        black = Move("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
      )
    )
  )

  val pgn6 = Pgn(
    tags = Tags(
      List(
        Tag(_.Result, "0-1")
      )
    ),
    turns = List()
  )

  val pgn7 = Pgn(
    tags = Tags.empty,
    turns = List()
  )

  val pgn8 = Pgn(
    tags = Tags.empty,
    turns = List(),
    initial = Initial(List("Why hello there!"))
  )

  val pgn9 = Pgn(
    tags = Tags.empty,
    turns = List(),
    initial = Initial(
      List(
        "Why hello there!",
        "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
      )
    )
  )

  val pgn10 = Pgn(
    tags = Tags.empty,
    turns = List(
      Turn(
        number = 1,
        white = Move(
          "d4",
          variations = List(
            List(
              Turn(
                number = 1,
                white = Move("e4").some,
                black = None
              )
            )
          )
        ).some,
        black = Move(
          "Nf6",
          variations = List(
            List(
              Turn(
                number = 1,
                white = None,
                black = Move("d5").some
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

  val pgn11 = Pgn(
    tags = Tags(
      List(
        Tag(_.TimeControl, "\"")
      )
    ),
    turns = List()
  )

  val pgn12 = Pgn(
    tags = Tags(
      List(
        Tag(_.TimeControl, "\\")
      )
    ),
    turns = List()
  )

  val pgns = List(pgn1, pgn2, pgn3, pgn4, pgn5, pgn6, pgn7, pgn8, pgn9, pgn10, pgn11, pgn12)
  // val pgns = List(pgn4, pgn5, pgn6)//, pgn7, pgn8, pgn9, pgn10, pgn11, pgn12)

  // test("NewPgn and Pgn are isomorphic"):
  //   pgns.foreach { pgn =>
  //     val newPgn = NewPgn(pgn)
  //     assertEquals(pgn, newPgn.toPgn)
  //   }

  test("NewPgn and Pgn PgnStr"):
    pgns.foreach { pgn =>
      val newPgn = NewPgn(pgn)
      assertEquals(pgn.toString, newPgn.render.value)
    }
