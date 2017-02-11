package chess

import variant.Horde

class HordeVariantTest extends ChessTest {

  "Horde chess" should {

    "Must recognise insufficient winning material for white with 1 pawn left." in {
      val position = "8/2k5/3q4/8/8/8/1P6/8 w - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case gm =>
          gm.situation.board.variant.insufficientWinningMaterial(gm.situation.board, Color.white) must beTrue
      }
    }

    "Must recognise insufficient winning material for white with 1 queen left." in {
      val position = "8/2k5/3q4/8/8/1Q6/8/8 w - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case gm =>
          gm.situation.board.variant.insufficientWinningMaterial(gm.situation.board, Color.white) must beTrue
      }

    }

    "Must recognise insufficient winning material for white with only 2 minor pieces left" in {
      val position = "8/2k5/3q4/8/8/1B2N3/8/8 w - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case gm =>
          gm.situation.board.variant.insufficientWinningMaterial(gm.situation.board, Color.white) must beTrue
      }
    }

    "Must not be insufficient winning material with 3 minor pieces left" in {
      val position = "8/2k5/3q4/8/8/3B4/4NB2/8 w - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case gm =>
          gm.situation.board.variant.insufficientWinningMaterial(gm.situation.board, Color.white) must beFalse
      }
    }

    "Must not auto-draw in B vs K endgame, black can win" in {
      val position = "7B/6k1/8/8/8/8/8/8 b - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case gm =>
          gm.situation.board.variant.insufficientWinningMaterial(gm.situation.board) must beFalse
      }
    }
  }
}
