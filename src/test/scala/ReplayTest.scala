package chess

import Pos._

class ReplayTest extends ChessTest {

  import format.pgn.Fixtures._

  "from prod" in {
    "replay from position close chess" in {
      val fen = """8/rnbqkbnr/pppppppp/8/8/PPPPPPPP/RNBQKBNR/8 w - - 0 1"""
      val moves = """d4 d5 Nf4 Nf5 g4 g5 gxf5 exf5""".split(' ').toList
      Replay.gameWhileValid(moves, fen, variant.FromPosition) must beLike {
        case (games, None) =>
          games.size must_== 9
        case (games, Some(err)) =>
          println(err)
          println(games.head.board)
          games.size must_== 9
      }
    }
  }
}
