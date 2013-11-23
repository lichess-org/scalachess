package chess
package format.pgn

import Pos._

class RenderTest extends ChessTest {

  /*
[Event "WCh"]
[Site "Bonn GER"]
[Date "2008.10.14"]
[Round "1"]
[White "Kramnik,V"]
[Black "Anand,V"]
[Result "1/2-1/2"]
[WhiteElo "2772"]
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
        tags = List(
          Tag(_.White, "Kramnik,V"),
          Tag(_.Black, "Anand,V"),
          Tag(_.ECO, "D14")
        ),
          turns = List(
            Turn(
              number = 1,
              white = Move("d4").some,
              black = Move("d5").some
            ),
            Turn(
              number = 2,
              white = Move("c4", nag = 1.some).some,
              black = Move("c6", nag = 2.some).some
            ),
            Turn(
              number = 3,
              white = Move("Nc3", nag = 3.some).some,
              black = Move("Nf6", nag = 4.some).some
            ),
            Turn(
              number = 4,
              white = Move("cxd5", nag = 5.some, comment = "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one".some).some,
              black = Move("cxd5", nag = 6.some).some
            ),
            Turn(
              number = 5,
              white = Move("Bf4").some,
              black = Move("Nc6").some
            )
          )
      )
      pgn.toString must_== """[White "Kramnik,V"]
[Black "Anand,V"]
[ECO "D14"]

1. d4 d5 2. c4! c6? 3. Nc3!! Nf6?? 4. cxd5 $5 { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one } 4... cxd5?! 5. Bf4 Nc6"""
    }
    "be correct when there are move times" in {
      val pgn = Pgn(
        tags = List(
          Tag(_.White, "tsinnema"),
          Tag(_.Black, "stockfish"),
          Tag(_.TimeControl, "300"),
          Tag(_.ECO, "A00e")
        ),
          turns = List(
            Turn(
              number = 1,
              white = Move("a4",timeLeft = 298.some).some,
              black = Move("Nf6", timeLeft = 299.some).some
            ),
            Turn(
              number = 2,
              white = Move("d4", timeLeft = 295.some).some,
              black = Move("d5", timeLeft = 298.some).some
            ),
            Turn(
              number = 3,
              white = Move("h4", timeLeft= 292.some).some,
              black = Move("e6", timeLeft = 297.some).some
            ),
            Turn(
              number = 4,
              white = Move("Qd3", nag = 3.some, timeLeft = 288.some,
                comment = "An invention of true genius.".some).some,
              black = Move("c5", timeLeft =  296.some).some
            ),
            Turn(
              number = 5,
              white = Move("dxc5", timeLeft = 258.some).some,
              black = Move("Bxc5", nag = 1.some, timeLeft=295.some).some
            )
          )
      )
      pgn.toString must_== """[White "tsinnema"]
[Black "stockfish"]
[TimeControl "300"]
[ECO "A00e"]

1. a4 { [%clk 0:04:58] } Nf6 { [%clk 0:04:59] } 2. d4 { [%clk 0:04:55] } d5 { [%clk 0:04:58] } 3. h4 { [%clk 0:04:52] } e6 { [%clk 0:04:57] } 4. Qd3!! { [%clk 0:04:48] An invention of true genius. } 4... c5 { [%clk 0:04:56] } 5. dxc5 { [%clk 0:04:18] } Bxc5! { [%clk 0:04:55] }"""
    }
  }
}
