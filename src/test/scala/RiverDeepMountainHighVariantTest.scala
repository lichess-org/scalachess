package chess

import chess.Pos._
import chess.format.FEN
import chess.variant.RiverDeepMountainHigh
import org.specs2.matcher.ValidatedMatchers

class RiverDeepMountainHighVariantTest extends ChessTest with ValidatedMatchers {

  "RiverDeepMountainHigh " should {

    "Not allow a knight to be move to the river" in {
      val game = Game(RiverDeepMountainHigh).playMoves((E2, E4), (F7, F5))

      val invalidGame = game flatMap (_.playMove(B1, A3))
      invalidGame must beInvalid("Piece on b1 cannot move to a3")
    }

    "Not allow a knight to checkmate on the river" in {
      val position = FEN("7k/8/6N1/8/8/8/8/K7 w")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val invalidGame = game flatMap (_.playMove(G6, H8))
      invalidGame must beInvalid("Piece on g6 cannot move to h8")
    }

    "Allow a knight to ignore checkmate on the river" in {
      val position = FEN("7k/8/6N1/8/8/8/8/K7 w")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val validGame = game flatMap (_.playMove(G6, E5))
      validGame must beValid
    }

    "Not allow the Rook to pass through the mountain horizontally" in {
      val position = FEN("7k/8/8/8/R7/8/8/7K")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val invalidGame = game flatMap (_.playMove(A4, F4))
      invalidGame must beInvalid("Piece on a4 cannot move to f4")
    }

    "Not allow the Rook to pass through the mountain vertically" in {
      val position = FEN("7k/3R4/8/8/8/8/8/7K")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val invalidGame = game flatMap (_.playMove(D7, D2))
      invalidGame must beInvalid("Piece on d7 cannot move to d2")
    }

    "Not allow the Rook to land on the mountain horizontally" in {
      val position = FEN("7k/8/8/8/R7/8/8/7K")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val invalidGame = game flatMap (_.playMove(A4, D4))
      invalidGame must beInvalid("Piece on a4 cannot move to d4")
    }

    "Not allow the Rook to land on the mountain vertically" in {
      val position = FEN("7k/3R4/8/8/8/8/8/7K")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val invalidGame = game flatMap (_.playMove(D7, D5))
      invalidGame must beInvalid("Piece on d7 cannot move to d5")
    }

    "Allow the Rook to move outside the mountain horizontally" in {
      val position = FEN("7k/8/8/8/8/R7/8/7K")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val validGame = game flatMap (_.playMove(A3, E3))
      validGame must beValid
    }

    "Allow the Rook to move outside the mountain vertically" in {
      val position = FEN("7k/2R5/8/8/8/8/8/7K")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val validGame = game flatMap (_.playMove(C7, C4))
      validGame must beValid
    }

    "Allow the Queen to land on the mountain vertically" in {
      val position = FEN("7k/3Q4/8/8/8/8/8/7K")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val validGame = game flatMap (_.playMove(D7, D5))
      validGame must beValid
    }

    "Allow the Queen to pass through the mountain vertically" in {
      val position = FEN("7k/3Q4/8/8/8/8/8/7K")
      val game     = fenToGame(position, RiverDeepMountainHigh)

      val validGame = game flatMap (_.playMove(D7, D3))
      validGame must beValid
    }
  }
}
