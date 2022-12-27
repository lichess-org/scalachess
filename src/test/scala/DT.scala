package chess


import cats.syntax.option.*
import org.specs2.matcher.ValidatedMatchers
import chess.format.{ EpdFen, Fen }
import chess.format.pgn.Reader
import chess.variant.Antichess

import chess.format.EpdFen
import chess.Pos.*
import chess.variant.*

class DT extends ChessTest:

  "racingKings" should {

    "Not allow a king to be check mated" in {
      val game = Game(Antichess).playMoves(
        Pos.F2 -> Pos.F3,
        Pos.E7 -> Pos.E6,
        Pos.G2 -> Pos.G4,
        Pos.D8 -> Pos.H4
      )

      game must beValid.like { case newGame =>
        println(s" game valid ${newGame.situation.moves.values}")
        newGame.situation.checkMate must beFalse
      }
    }

  }
