package chess

import variant.Chess960

import chess.format.pgn.Reader
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
      Fen
        .read(Chess960, EpdFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1"))
        .map(_.board.history.unmovedRooks) must beSome(360287970189639685L)
    }

    "UnmovedRooks with board init" in {
      val pieces = Chess960.pieces
      Board(pieces, Chess960.castles, Chess960).history.unmovedRooks must beEqualTo(
        bitboard.Board.fromMap(pieces).rooks
      )
    }

    "Castles when a1 is being taken" in {
      val pgn = """
[Event "Hourly Chess960 Arena"]
[Site "https://lichess.org/guOuBSra"]
[Date "2023.01.20"]
[White "e4_d5_cxd5"]
[Black "Cpt_Wrongel"]
[Result "0-1"]
[UTCDate "2023.01.20"]
[UTCTime "06:13:12"]
[WhiteElo "1951"]
[BlackElo "1956"]
[WhiteRatingDiff "-6"]
[BlackRatingDiff "+8"]
[Variant "Chess960"]
[TimeControl "300+0"]
[ECO "?"]
[Opening "?"]
[Termination "Normal"]
[FEN "brnqknrb/pppppppp/8/8/8/8/PPPPPPPP/BRNQKNRB w KQkq - 0 1"]
[SetUp "1"]
[Annotator "lichess.org"]
1. d4 g6 2. e4 b6 3. g3 f5 4. exf5 Bxh1 5. Rxh1 gxf5 6. Qh5+ Rg6 7. Qxf5 Nd6 8. Qd3 Ne6 9. Ne2 c5 10. b3 Qc7 11. d5 Bxa1 12. dxe6 Bf6 13. exd7+ Qxd7 14. Ne3 O-O-O
      """

      val game = Reader.full(pgn)
      game must beValid.like { case Reader.Result.Complete(replay) =>
        val game = replay.state
        game.situation.legalMoves.find(_.castles) must beSome
      }
    }

  }
