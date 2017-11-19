package chess

import variant.Horde

class HordeVariantTest extends ChessTest {

  "Horde chess" should {

    "Must recognise insufficient winning material for horde with simple fortress." in {
      val position = "8/pk6/P7/P7/P7/P7/P7/P7 b - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case game =>
          game.situation.board.variant.insufficientWinningMaterial(game.situation.board, Color.white) must beTrue
      }
    }

    "Must recognise insufficient winning material for horde with only 1 pawn left." in {
      val position = "8/2k5/3q4/8/8/8/1P6/8 b - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case game =>
          game.situation.board.variant.insufficientWinningMaterial(game.situation.board, Color.white) must beTrue
      }
    }

    "Must recognise insufficient winning material for horde with only 1 queen left." in {
      val position = "8/2k5/3q4/8/8/1Q6/8/8 b - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case game =>
          game.situation.board.variant.insufficientWinningMaterial(game.situation.board, Color.white) must beTrue
      }

    }

    "Must recognise insufficient winning material for horde with only 2 minor pieces left" in {
      val position = "8/2k5/3q4/8/8/1B2N3/8/8 b - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case game =>
          game.situation.board.variant.insufficientWinningMaterial(game.situation.board, Color.white) must beTrue
      }
    }

    "Must not be insufficient winning material for horde with 3 minor pieces left" in {
      val position = "8/2k5/3q4/8/8/3B4/4NB2/8 b - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case game =>
          game.situation.board.variant.insufficientWinningMaterial(game.situation.board, Color.white) must beFalse
      }
    }

    "Must not auto-draw in B vs K endgame, king can win" in {
      val position = "7B/6k1/8/8/8/8/8/8 b - -"
      val game = fenToGame(position, Horde)

      game must beSuccess.like {
        case game =>
          game.situation.board.variant.insufficientWinningMaterial(game.situation.board) must beFalse
      }
    }
  }
}
