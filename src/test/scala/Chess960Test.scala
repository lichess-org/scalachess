package chess

import variant.Chess960

import chess.format.EpdFen
import chess.format.Fen

class Chess960Test extends ChessTest:

  "Chess960 chess" should {

    "recognize position numbers" in {
      Chess960 positionNumber EpdFen("k7/ppP5/brp5/8/8/8/8/8 b - -") must beNone

      Chess960 positionNumber EpdFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w KQkq - 0 1") must beSome(
        521
      )
      Chess960 positionNumber EpdFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNBBBKNR w KQkq - 0 1") must beNone
      Chess960 positionNumber EpdFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNqBBKNR w KQkq - 0 1") must beNone
      Chess960 positionNumber EpdFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR b KQkq - 0 1") must beNone
      Chess960 positionNumber EpdFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w Kkq - 0 1") must beNone
      Chess960 positionNumber EpdFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w KQkq - 1 1") must beNone

      Chess960 positionNumber EpdFen("bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w KQkq - 0 1") must beSome(
        0
      )
      Chess960 positionNumber EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1") must beSome(
        959
      )

      Chess960 positionNumber EpdFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w AHa - 0 1") must beNone
      Chess960 positionNumber EpdFen("bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w AHah - 0 1") must beNone
    }

    "UnmovedRooks with initial fen" in {
      Fen.read(Chess960, EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1")).map(_.board.history.unmovedRooks) must beSome(360287970189639685L)
    }

    "UnmovedRooks with board init" in {
      val pieces = Chess960.pieces
      Board(pieces, Chess960.castles, Chess960).history.unmovedRooks must beEqualTo(bitboard.Board.fromMap(pieces).rooks)
    }
  }
