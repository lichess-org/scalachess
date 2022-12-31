package chess

import variant.Horde
import chess.format.EpdFen

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

    "Must not auto-draw in B vs K endgame, king can win" in {
      val position = EpdFen("7B/6k1/8/8/8/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Hord pawn on first rank should able to move two squares" in {
      val position = EpdFen("8/pp1k2q1/3P2p1/8/P3PP2/PPP2r2/PPP5/PPPP4 w - - 1 2")
      val game     = fenToGame(position, Horde)
      game must beValid.like {case game =>
        game.situation.allTrustedMoves.exists(m => m.orig == Pos.D1 && m.dest == Pos.D3)
      }
    }

  }
