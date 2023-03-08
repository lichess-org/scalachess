package chess

import variant.Horde
import chess.format.EpdFen
import chess.format.pgn.SanStr

class HordeVariantTest extends ChessTest:

  "Horde chess" should {

    "Must not be insufficient winning material for horde with only 1 pawn left" in {
      val position = EpdFen("k7/ppP5/brp5/8/8/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must recognise insufficient winning material for horde with only 1 pawn left" in {
      val position = EpdFen("8/2k5/3q4/8/8/8/1P6/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must not be insufficient winning material for king with only 1 pawn left" in {
      val position = EpdFen("8/2k5/3q4/8/8/8/1P6/8 w - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must recognise insufficient winning material for horde with only 1 bishop left" in {
      val position = EpdFen("r7/2Bb4/q3k3/8/8/3q4/8/5qqr b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.end must beFalse
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must recognise insufficient winning material for horde with only 1 queen left" in {
      val position = EpdFen("8/2k5/3q4/8/8/1Q6/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must not be insufficient winning material for king with only 1 queen left" in {
      val position = EpdFen("8/2k5/3q4/8/8/1Q6/8/8 w - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must recognise insufficient winning material for horde with only 2 minor pieces left" in {
      val position = EpdFen("8/2k5/3q4/8/8/1B2N3/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must not be insufficient winning material for king with only 2 minor pieces left" in {
      val position = EpdFen("8/2k5/3q4/8/8/1B2N3/8/8 w - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must not be insufficient winning material for horde with 3 minor pieces left" in {
      val position = EpdFen("8/2k5/3q4/8/8/3B4/4NB2/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must not be insufficient winning material for king with queen and rook left" in {
      val position = EpdFen("8/5k2/7q/7P/6rP/6P1/6P1/8 b - - 0 52")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
        game.situation.autoDraw must beFalse
      }
    }

    "Must auto-draw in simple pawn fortress" in {
      val position = EpdFen("8/p7/pk6/P7/P7/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must auto-draw if horde is stalemated and only king can move" in {
      val position = EpdFen("QNBRRBNQ/PPpPPpPP/P1P2PkP/8/8/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must auto-draw if horde is stalemated and only king can move" in {
      val position = EpdFen("b7/pk6/P7/P7/8/8/8/8 b - - 0 1")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must not auto-draw if horde is not stalemated after the only king move" in {
      val position = EpdFen("8/1b5r/1P6/1Pk3q1/1PP5/r1P5/P1P5/2P5 b - - 0 52")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must not auto-draw in B vs K endgame, king can win" in {
      val position = EpdFen("7B/6k1/8/8/8/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Pawn on first rank should able to move two squares" in {
      val position = EpdFen("8/pp1k2q1/3P2p1/8/P3PP2/PPP2r2/PPP5/PPPP4 w - - 1 2")
      val game     = fenToGame(position, Horde)
      game must beValid.like { case game =>
        game.situation.legalMoves.exists(m => m.orig == Pos.D1 && m.dest == Pos.D3) must beTrue
      }
    }

    "Cannot en passant a pawn from the first rank" in {
      val position = EpdFen("k7/5p2/4p2P/3p2P1/2p2P2/1p2P2P/p2P2P1/2P2P2 w - - 0 1")
      val game     = fenToGame(position, Horde)
      val newGame  = game.flatMap(_.apply(Pos.C1, Pos.C3))
      newGame must beValid.like { case game =>
        game._1.situation.legalMoves.exists(m => m.orig == Pos.B3 && m.dest == Pos.C2) must beFalse
      }
    }

    "Castle with one rook moved" in {
      val sans = SanStr from "a5 h5 a4 Nc6 a3 b6 a2 Bb7 d5 d6 d4 Rh6 cxd6 Qxd6 f6"
        .split(' ')
        .toVector
      val (game, steps, error) = chess.Replay.gameMoveWhileValid(sans, Horde.initialFen, Horde)
      error must beNone
      steps.last._1.situation.legalMoves.exists(_.castles) must_== true
    }

    "UnmovedRooks & castles at the starting position" in {
      val board = Board.init(Horde)
      board.history.unmovedRooks must_== UnmovedRooks(Set(Pos.A8, Pos.H8))
      board.history.castles must_== Castles("kq")
    }

    "the h8 rooks move" in {
      val position = EpdFen("r3kbnr/p1pqppp1/1pnp3P/PPPP1P1P/PPP1PPP1/1PPP1PPP/PPPPPPPP/PPPPPPPP b kq - 0 7")
      val game     = fenToGame(position, Horde)
      val newGame  = game.flatMap(_.apply(Pos.H8, Pos.H6))
      newGame must beValid.like { case game =>
        game._1.situation.board.history.unmovedRooks must_== UnmovedRooks(Set(Pos.A8))
        game._1.situation.board.history.castles must_== Castles("q")
      }
    }

  }
