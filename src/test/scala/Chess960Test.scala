package chess

import variant.Chess960

import chess.format.FEN

class Chess960Test extends ChessTest {

  "Chess960 chess" should {

    "recognize position numbers" in {
      Chess960 positionNumber FEN("k7/ppP5/brp5/8/8/8/8/8 b - -") must beNone

      Chess960 positionNumber FEN("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR r KQkq - 0 1") must beSome(521)
      Chess960 positionNumber FEN("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNBBBKNR r KQkq - 0 1") must beNone
      Chess960 positionNumber FEN("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNqBBKNR r KQkq - 0 1") must beNone
      Chess960 positionNumber FEN("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR b KQkq - 0 1") must beNone
      Chess960 positionNumber FEN("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR r Kkq - 0 1") must beNone
      Chess960 positionNumber FEN("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR r KQkq - 1 1") must beNone

      Chess960 positionNumber FEN("bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR r KQkq - 0 1") must beSome(0)
      Chess960 positionNumber FEN("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB r KQkq - 0 1") must beSome(959)

      Chess960 positionNumber FEN("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR r AHa - 0 1") must beNone
      Chess960 positionNumber FEN("bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR r AHah - 0 1") must beNone
    }
  }
}
