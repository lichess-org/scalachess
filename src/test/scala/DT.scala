package chess

// import org.specs2.matcher.MustExpectations.akaMust

import chess.variant.FromPosition
import chess.Pos.*

class DT extends ChessTest:

  "castle" should {

    "from position with chess960 castling" in {
      val game = Game(
        makeBoard(
          """rk  r
pppbnppp
   p  n
P  Pp
    P  q
R     NP
 PP  PP
 KNQRB""",
          FromPosition
        ),
        Black
      )
      "dests" in {
        game.board destsFrom B8 must bePoss(A8, C8, E8)
      }
    }
  }
