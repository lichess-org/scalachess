package chess

import cats.syntax.option.*
import format.{ EpdFen, Fen, Uci }
import chess.format.pgn.SanStr
import chess.variant.Chess960

class ReplayTest extends ChessTest:

  "from prod" in {
    "replay from position close chess" in {
      val fen   = EpdFen("""8/rnbqkbnr/pppppppp/8/8/PPPPPPPP/RNBQKBNR/8 w - - 0 1""")
      val moves = SanStr from """d4 d5 Nf4 Nf5 g4 g5 gxf5 exf5""".split(' ').toList
      Replay.gameMoveWhileValid(moves, fen, variant.FromPosition) must beLike { case (_, games, None) =>
        games.size must_== 8
        games(1)._2._2 === "d5"
      }
    }
  }

  "castle rights" in {
    "bongcloud attack" in {
      Replay.situationsFromUci(
        moves = List(Uci("e2e4"), Uci("e7e5"), Uci("e1e2")).flatten,
        initialFen = None,
        variant = variant.Standard
      ) must beValid.like { situations =>
        situations.map(Fen.write) must_== List(
          EpdFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
          EpdFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"),
          EpdFen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1"),
          EpdFen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPPKPPP/RNBQ1BNR b kq - 1 1")
        )
      }
    }
  }

  "variant situations" in {
    "racing kings" in {
      Replay.situations(
        sans = SanStr.from("Be3 Ne4 Rg3 Nxe3 Rxe3" split " "),
        initialFen = None,
        variant = chess.variant.RacingKings
      ) must beValid
    }
  }

  "chess960 castlings" in {
    val sans: Vector[SanStr] =
      SanStr from "e4 e5 Ne3 f6 Nb3 Nb6 O-O-O Ne6 Ba6 O-O-O Nd5 Nxd5 Bxb7+ Kb8"
        .split(' ')
        .toVector
    val (game, steps, error) = chess.Replay.gameMoveWhileValid(
      sans,
      Fen.Epd("nrknbbqr/pppppppp/8/8/8/8/PPPPPPPP/NRKNBBQR w KQkq - 0 1"),
      Chess960
    )
    error must beNone
    steps.size === sans.size
  }

  "castling always 960 notation" in {
    val sans: Vector[SanStr] =
      SanStr from "d4 Nf6 c4 g6 Nc3 Bg7 e4 d6 f3 O-O Be3 e5 d5 Nh5 Qd2 Qh4+ g3 Qe7 O-O-O"
        .split(' ')
        .toVector
    val (game, steps, error) = chess.Replay.gameMoveWhileValid(
      sans,
      Fen.Epd.initial,
      variant.Standard
    )
    error must beNone
    steps.size must_== sans.size
    steps(9)._2.uci.uci must_== "e8h8"
    steps(18)._2.uci.uci must_== "e1a1"
  }
