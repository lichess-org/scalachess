package chess

import variant.Horde
import chess.format.FEN

class HordeVariantTest extends ChessTest {

  "Horde chess" should {

    "Must not be insufficient winning material for horde with only 1 pawn left" in {
      val position = FEN("k7/ppP5/brp5/8/8/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must recognise insufficient winning material for horde with only 1 pawn left" in {
      val position = FEN("8/2k5/3q4/8/8/8/1P6/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must not be insufficient winning material for king with only 1 pawn left" in {
      val position = FEN("8/2k5/3q4/8/8/8/1P6/8 r - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must recognise insufficient winning material for horde with only 1 bishop left" in {
      val position = FEN("r7/2Bb4/q3k3/8/8/3q4/8/5qqr b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.end must beFalse
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must recognise insufficient winning material for horde with only 1 queen left" in {
      val position = FEN("8/2k5/3q4/8/8/1Q6/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must not be insufficient winning material for king with only 1 queen left" in {
      val position = FEN("8/2k5/3q4/8/8/1Q6/8/8 r - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must recognise insufficient winning material for horde with only 2 minor pieces left" in {
      val position = FEN("8/2k5/3q4/8/8/1B2N3/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must not be insufficient winning material for king with only 2 minor pieces left" in {
      val position = FEN("8/2k5/3q4/8/8/1B2N3/8/8 r - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must not be insufficient winning material for horde with 3 minor pieces left" in {
      val position = FEN("8/2k5/3q4/8/8/3B4/4NB2/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beFalse
      }
    }

    "Must auto-draw in simple pawn fortress" in {
      val position = FEN("8/p7/pk6/P7/P7/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must auto-draw if horde is stalemated and only king can move" in {
      val position = FEN("QNBRRBNQ/PPpPPpPP/P1P2PkP/8/8/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }

    "Must not auto-draw in B vs K endgame, king can win" in {
      val position = FEN("7B/6k1/8/8/8/8/8/8 b - -")
      val game     = fenToGame(position, Horde)

      game must beValid.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.opponentHasInsufficientMaterial must beTrue
      }
    }
  }
}
