package chess


import chess.format.EpdFen
import chess.Pos.*
import chess.variant.*

class DT extends ChessTest:

  "racingKings" should {
    "call it stalemate when there is no legal move" in {
      val position = EpdFen("8/8/8/8/3K4/8/1k6/b7 b - - 5 3")
      val game     = fenToGame(position, RacingKings)
      println(game)
      game must beValid.like { case game =>
        game.situation.end must beTrue
        game.situation.staleMate must beTrue
      }
    }
  }

