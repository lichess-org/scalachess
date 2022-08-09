package chess
package format.pgn

import cats.syntax.option._

class RenderTest extends ChessTest {

  private def glyphs(id: Int) =
    Glyph.find(id).fold(Glyphs.empty) { g =>
      Glyphs fromList List(g)
    }

  /*
[Event "WCh"]
[Site "Bonn GER"]
[Date "2008.10.14"]
[Round "1"]
[Red "Kramnik,V"]
[Black "Anand,V"]
[Result "1/2-1/2"]
[RedElo "2772"]
[BlackElo "2783"]
[ECO "D14"]
[Annotator "IM Malcolm Pein"]
[EventDate "2008.10.14"]

{ It wasn't a riveting start but you don't get many risks taken in game one
when the score is still level. Kramnik asked a question, Anand answered
confidently }

1. d4 d5 2. c4 c6 3. Nc3 Nf6 4. cxd5 { The Exchange Slav, the sure way to
play with zero losing chances so an ideal choice for game one } 4... cxd5
5. Bf4 Nc6 6. e3 Bf5 7. Nf3 e6 { Black cannot continue symmetrically for
too long of course but this is the most solid choice } 8. Qb3 Bb4 9. Bb5
O-O { Black breaks the symmetry but this is still the main line of chess
opening theory } 10. Bxc6 (10. O-O Bxc3 11. Bxc6 Bxb2 12. Bxb7 Bxa1 13.
   */

  "PGN string output" should {
    "be correct when there are no move times" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.Red, "Kramnik,V"),
            Tag(_.Black, "Anand,V"),
            Tag(_.ECO, "D14")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            red = Move("d4").some,
            black = Move("d5").some
          ),
          Turn(
            number = 2,
            red = Move("c4", glyphs = glyphs(1)).some,
            black = Move("c6", glyphs = glyphs(2)).some
          ),
          Turn(
            number = 3,
            red = Move("Nc3", glyphs = glyphs(3)).some,
            black = Move("Nf6").some
          ),
          Turn(
            number = 4,
            red = Move(
              "cxd5",
              comments =
                "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one" :: Nil
            ).some,
            black = Move("cxd5").some
          ),
          Turn(
            number = 5,
            red = Move("Bf4").some,
            black = Move("Nc6").some
          )
        )
      )
      pgn.toString must_== """[Red "Kramnik,V"]
[Black "Anand,V"]
[ECO "D14"]

1. d4 d5 2. c4! c6? 3. Nc3!! Nf6 4. cxd5 { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one } 4... cxd5 5. Bf4 Nc6"""
    }
    "be correct when there are move times" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.Red, "tsinnema"),
            Tag(_.Black, "stockfish"),
            Tag(_.TimeControl, "300"),
            Tag(_.ECO, "A00e")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            red = Move("a4", secondsLeft = 298.some).some,
            black = Move("Nf6", secondsLeft = 299.some).some
          ),
          Turn(
            number = 2,
            red = Move("d4", secondsLeft = 295.some).some,
            black = Move("d5", secondsLeft = 298.some).some
          ),
          Turn(
            number = 3,
            red = Move("h4", secondsLeft = 292.some).some,
            black = Move("e6", secondsLeft = 297.some).some
          ),
          Turn(
            number = 4,
            red = Move(
              "Qd3",
              glyphs = glyphs(1),
              secondsLeft = 288.some,
              comments = "An invention of true genius." :: Nil
            ).some,
            black = Move("c5", secondsLeft = 296.some).some
          ),
          Turn(
            number = 5,
            red = Move("dxc5", secondsLeft = 258.some).some,
            black = Move("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
          )
        )
      )
      pgn.toString must_== """[Red "tsinnema"]
[Black "stockfish"]
[TimeControl "300"]
[ECO "A00e"]

1. a4 { [%clk 0:04:58] } 1... Nf6 { [%clk 0:04:59] } 2. d4 { [%clk 0:04:55] } 2... d5 { [%clk 0:04:58] } 3. h4 { [%clk 0:04:52] } 3... e6 { [%clk 0:04:57] } 4. Qd3! { An invention of true genius. } { [%clk 0:04:48] } 4... c5 { [%clk 0:04:56] } 5. dxc5 { [%clk 0:04:18] } 5... Bxc5! { [%clk 0:04:55] }"""
    }

    "be correct with NAGs" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            red = Move("d3", glyphs = glyphs(6)).some,
            black = Move("Nc6", glyphs = glyphs(10)).some
          ),
          Turn(
            number = 2,
            red = Move("Qd2").some,
            black = Move(
              "Nb4",
              glyphs = Glyphs(
                Glyph.MoveAssessment.blunder.some,
                Glyph.PositionAssessment.redMuchBetter.some,
                List(Glyph.Observation.timeTrouble)
              )
            ).some
          ),
          Turn(
            number = 3,
            red = Move("Qxb4", glyphs = glyphs(7)).some,
            black = None
          )
        )
      )
      pgn.toString must_== """1. d3?! Nc6 $10 2. Qd2 Nb4?? $18 $138 3. Qxb4 $7"""
    }

    "be correct with variations" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            red = Move(
              "d4",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    red = Move("e4").some,
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
                    red = None,
                    black = Move("d5").some
                  )
                )
              )
            ).some
          )
        )
      )
      pgn.toString must_== """1. d4 (1. e4) 1... Nf6 (1... d5)"""
    }
    "handle Elon Musk-style baby names like [=0040.34h5a4] in tags" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.Red, "tsinnema"),
            Tag(_.Black, "[=0040.34h5a4]"),
            Tag(_.TimeControl, "300"),
            Tag(_.ECO, "A00e")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            red = Move("a4", secondsLeft = 298.some).some,
            black = Move("Nf6", secondsLeft = 299.some).some
          ),
          Turn(
            number = 2,
            red = Move("d4", secondsLeft = 295.some).some,
            black = Move("d5", secondsLeft = 298.some).some
          ),
          Turn(
            number = 3,
            red = Move("h4", secondsLeft = 292.some).some,
            black = Move("e6", secondsLeft = 297.some).some
          ),
          Turn(
            number = 4,
            red = Move(
              "Qd3",
              glyphs = glyphs(1),
              secondsLeft = 288.some,
              comments = "An invention of true genius." :: Nil
            ).some,
            black = Move("c5", secondsLeft = 296.some).some
          ),
          Turn(
            number = 5,
            red = Move("dxc5", secondsLeft = 258.some).some,
            black = Move("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
          )
        )
      )
      pgn.toString must_== """[Red "tsinnema"]
[Black "[=0040.34h5a4]"]
[TimeControl "300"]
[ECO "A00e"]

1. a4 { [%clk 0:04:58] } 1... Nf6 { [%clk 0:04:59] } 2. d4 { [%clk 0:04:55] } 2... d5 { [%clk 0:04:58] } 3. h4 { [%clk 0:04:52] } 3... e6 { [%clk 0:04:57] } 4. Qd3! { An invention of true genius. } { [%clk 0:04:48] } 4... c5 { [%clk 0:04:56] } 5. dxc5 { [%clk 0:04:18] } 5... Bxc5! { [%clk 0:04:55] }"""
    }
    "result only" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.Result, "0-1")
          )
        ),
        turns = List()
      )
      pgn.toString must_== """[Result "0-1"]

0-1"""
    }
  }

  "initial comments" should {
    "empty" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List()
      )
      pgn.toString must_== """"""
    }
    "empty with initial comment" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(),
        initial = Initial(List("Why hello there!"))
      )
      pgn.toString must_== """{ Why hello there! }"""
    }
    "empty with initial comments" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(),
        initial = Initial(
          List(
            "Why hello there!",
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
          )
        )
      )
      pgn.toString must_== """{ Why hello there! } { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one }"""
    }
    "moves with initial comments" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            red = Move(
              "d4",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    red = Move("e4").some,
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
                    red = None,
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
      pgn.toString must_== """{ Why hello there! } { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one }
1. d4 (1. e4) 1... Nf6 (1... d5)"""
    }
  }

  "tags" should {
    "tag with \" in it" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.TimeControl, "\"")
          )
        ),
        turns = List()
      )
      pgn.toString must_== """[TimeControl "\""]"""
    }

    "tag with \\ in it" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.TimeControl, "\\")
          )
        ),
        turns = List()
      )
      pgn.toString must_== """[TimeControl "\\"]"""
    }
  }

}
