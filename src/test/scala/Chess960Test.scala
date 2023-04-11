package chess

import scala.language.implicitConversions
import variant.Chess960
import chess.format.pgn.Reader
import chess.format.EpdFen
import chess.format.Fen

class Chess960Test extends ChessTest:

  "Chess960 chess" should:

    "recognize position numbers" in:
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

    "Castles when a1 is being taken" in:
      val pgn = """
[Variant "Chess960"]
[FEN "brnqknrb/pppppppp/8/8/8/8/PPPPPPPP/BRNQKNRB w KQkq - 0 1"]
1. d4 g6 2. e4 b6 3. g3 f5 4. exf5 Bxh1 5. Rxh1 gxf5 6. Qh5+ Rg6 7. Qxf5 Nd6 8. Qd3 Ne6 9. Ne2 c5 10. b3 Qc7 11. d5 Bxa1 12. dxe6 Bf6 13. exd7+ Qxd7 14. Ne3 O-O-O
      """

      Reader.full(pgn) must beValid.like { case Reader.Result.Complete(replay) =>
        replay.state.situation.legalMoves.find(_.castles).map(_.toUci) === Some(
          format.Uci.Move(Square.E1, Square.B1)
        )
      }
