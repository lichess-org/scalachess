package chess

import InsufficientMatingMaterial.*
import chess.format.EpdFen
import chess.variant.Standard

class InsufficientMatingMaterialTest extends ChessTest:

  "bishops on Opposite colors" should {

    val trues = List(
      "8/4b3/8/8/8/8/4B3/8 w - - 0 1",
      "8/4b3/8/8/2Q5/1K6/4B3/5B2 w - - 0 1",
      "5b2/1k2b1n1/3q4/8/2Q5/1K6/4B3/5B2 w - - 0 1",
      "6b1/1k3bn1/3q4/8/2Q5/1K6/4bB2/8 w - - 0 1",
      "6b1/1k3bn1/3B4/8/2Q2B2/1K6/4bB2/8 w - - 0 1",
      "2k2b2/5b2/8/8/8/3R4/1K2Q3/5B2 w - - 0 1",
      "2k2b2/6b1/7b/8/8/3R2B1/1K2Q3/5B2 w - - 0 1",
      "2k5/8/8/8/8/3R2B1/1K2Q3/5B2 w - - 0 1",
    ).map(EpdFen(_))

    val falses = List(
      "4b3/8/8/8/8/8/4B3/8 w - - 0 1",
      "5b2/8/8/8/8/3R4/1K2QB2/8 w - - 0 1",
      "8/8/8/8/8/3R4/1K2B3/8 w - - 0 1",
      "5b2/8/8/8/8/3R4/1K2Q3/8 w - - 0 1",
    ).map(EpdFen(_))

    "return true" in {
      forall(trues) { fen =>
        val maybeGame = fenToGame(fen, Standard)
        maybeGame must beValid.like { case game =>
          bishopsOnOppositeColors(game.situation.board) must_== true
        }
      }
    }

    "return false" in {
      forall(falses) { fen =>
        val maybeGame = fenToGame(fen, Standard)
        maybeGame must beValid.like { case game =>
          bishopsOnOppositeColors(game.situation.board) must_== false
        }
      }
    }

  }
