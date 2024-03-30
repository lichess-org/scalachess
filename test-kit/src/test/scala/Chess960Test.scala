package chess

import chess.format.FullFen
import chess.format.pgn.Reader

import scala.language.implicitConversions

import variant.Chess960

class Chess960Test extends ChessTest:

  test("recognize position numbers"):
    import Chess960.{ positionNumber as pn }
    assertEquals(pn(FullFen("k7/ppP5/brp5/8/8/8/8/8 b - -")), None)

    assertEquals(pn(FullFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w KQkq - 0 1")), Some(521))
    assertEquals(pn(FullFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNBBBKNR w KQkq - 0 1")), None)
    assertEquals(pn(FullFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNqBBKNR w KQkq - 0 1")), None)
    assertEquals(pn(FullFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR b KQkq - 0 1")), None)
    assertEquals(pn(FullFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w Kkq - 0 1")), None)
    assertEquals(pn(FullFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w KQkq - 1 1")), None)

    assertEquals(pn(FullFen("bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w KQkq - 0 1")), Some(0))
    assertEquals(pn(FullFen("rkrnnqbb/pppppppp/8/8/8/8/PPPPPPPP/RKRNNQBB w KQkq - 0 1")), Some(959))

    assertEquals(pn(FullFen("rnqbbknr/pppppppp/8/8/8/8/PPPPPPPP/RNQBBKNR w AHa - 0 1")), None)
    assertEquals(pn(FullFen("bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w AHah - 0 1")), None)

  test("Castles when a1 is being taken"):
    val pgn = """
[Variant "Chess960"]
[FEN "brnqknrb/pppppppp/8/8/8/8/PPPPPPPP/BRNQKNRB w KQkq - 0 1"]
1. d4 g6 2. e4 b6 3. g3 f5 4. exf5 Bxh1 5. Rxh1 gxf5 6. Qh5+ Rg6 7. Qxf5 Nd6 8. Qd3 Ne6 9. Ne2 c5 10. b3 Qc7 11. d5 Bxa1 12. dxe6 Bf6 13. exd7+ Qxd7 14. Ne3 O-O-O
      """

    Reader
      .full(pgn)
      .assertRight:
        case Reader.Result.Complete(replay) =>
          assertEquals(
            replay.state.situation.legalMoves.find(_.castles).map(_.toUci),
            Some(format.Uci.Move(Square.E1, Square.B1))
          )
