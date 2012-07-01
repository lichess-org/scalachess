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

  "RL annotated PGN" in {
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

1. d4 d5 2. c4 $1 c6 $2 3. Nc3 $3 Nf6 $4 4. cxd5 $5 { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one } 4... cxd5 $6 5. Bf4 Nc6"""
  }
}
