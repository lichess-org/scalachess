package chess

import variant.Chess960

import chess.format.Fen

class Chess960Test extends ChessTest:

  "Chess960 chess" should {

    "recognize position numbers" in {
      Chess960 positionNumber Fen("k7/ppP5/brp5/8/8/8/8/8 b - -") must beNone

      Chess960 positionNumber Fen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w KQkq - 0 1") must beSome(521)
      Chess960 positionNumber Fen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNBBBKNR w KQkq - 0 1") must beNone
      Chess960 positionNumber Fen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNqBBKNR w KQkq - 0 1") must beNone
      Chess960 positionNumber Fen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR b KQkq - 0 1") must beNone
      Chess960 positionNumber Fen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w Kkq - 0 1") must beNone
      Chess960 positionNumber Fen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w KQkq - 1 1") must beNone

      Chess960 positionNumber Fen("bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w KQkq - 0 1") must beSome(0)
      Chess960 positionNumber Fen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1") must beSome(959)

      Chess960 positionNumber Fen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w AHa - 0 1") must beNone
      Chess960 positionNumber Fen("bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w AHah - 0 1") must beNone
    }
  }
