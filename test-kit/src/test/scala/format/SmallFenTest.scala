package chess
package format

import scala.language.implicitConversions
import variant.Standard

class SmallFenTest extends ChessTest:

  import Square.*

  test("make standard correct"):
    assertEquals(
      SmallFen.make(Standard, Fen.Simple("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R b KQkq -")),
      SmallFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1RbKQkq")
    )
  test("make standard correct, skip w"):
    assertEquals(
      SmallFen.make(Standard, Fen.Simple("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R w KQkq -")),
      SmallFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1RKQkq")
    )
  test("make standard correct, non-initial castling"):
    assertEquals(
      SmallFen.make(Standard, Fen.Simple("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R w Qq -")),
      SmallFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1RQq")
    )
  test("make standard correct, en passant"):
    assertEquals(
      SmallFen
        .make(Standard, Fen.Simple("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R w KQkq d5")),
      SmallFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1RKQkqd5")
    )

  test("validate standard incorrect"):
    assertEquals(
      SmallFen.validate(Standard, Fen.Epd("this is not a pipe")),
      None
    )

  def toFen(simple: String) = SmallFen(simple).garbageInefficientReadBackIntoFen

  test("back to FEN"):
    assertEquals(
      toFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1RKQkq"),
      SimpleFen("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R w KQkq -")
    )

  test("back to FEN, black to play"):
    assertEquals(
      toFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1RbKQkq"),
      SimpleFen("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R b KQkq -")
    )

  test("back to FEN, non-initial castling rights"):
    assertEquals(
      toFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1RQq"),
      SimpleFen("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R w Qq -")
    )

  test("back to FEN, black to play, empty castling rights"):
    assertEquals(
      toFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1Rb"),
      SimpleFen("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R b - -")
    )

  test("back to FEN, en passant"):
    assertEquals(
      toFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1RKQkqd5"),
      SimpleFen("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R w KQkq d5")
    )

  test("back to FEN, black to play, no castling, en passant"):
    assertEquals(
      toFen("rnbqkb1rppp1pppp3p43nP33P45N2PPP2PPPRNBQKB1Rbd5"),
      SimpleFen("rnbqkb1r/ppp1pppp/3p4/3nP3/3P4/5N2/PPP2PPP/RNBQKB1R b - d5")
    )

  test("back and forth"):
    List(
      "1k6/6pp/8/8/8/8/3R4/3K4 w - -",
      "r6r/1b2k1bq/8/8/7B/8/8/R3K2R b KQ -",
      "8/8/8/2k5/2pP4/8/B7/4K3 b - d3",
      "r1bqkbnr/pppppppp/n7/8/8/P7/1PPPPPPP/RNBQKBNR w KQkq -",
      "r3k2r/p1pp1pb1/bn2Qnp1/2qPN3/1p2P3/2N5/PPPBBPPP/R3K2R b KQkq -",
      "2kr3r/p1ppqpb1/bn2Qnp1/3PN3/1p2P3/2N5/PPPBBPPP/R3K2R b KQ -",
      "rnb2k1r/pp1Pbppp/2p5/q7/2B5/8/PPPQNnPP/RNB1K2R w KQ -",
      "2r5/3pk3/8/2P5/8/2K5/8/8 w - -",
      "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ -",
      "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - -",
      "3k4/3p4/8/K1P4r/8/8/8/8 b - -",
      "8/8/4k3/8/2p5/8/B2P2K1/8 w - -",
      "8/8/1k6/2b5/2pP4/8/5K2/8 b - d3",
      "5k2/8/8/8/8/8/8/4K2R w K -",
      "3k4/8/8/8/8/8/8/R3K3 w Q -",
      "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq -",
      "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq -",
      "2K2r2/4P3/8/8/8/8/8/3k4 w - -",
      "8/8/1P2K3/8/2n5/1q6/8/5k2 b - -",
      "4k3/1P6/8/8/8/8/K7/8 w - -",
      "8/P1k5/K7/8/8/8/8/8 w - -",
      "K1k5/8/P7/8/8/8/8/8 w - -",
      "8/k1P5/8/1K6/8/8/8/8 w - -",
      "8/8/2k5/5q2/5n2/8/5K2/8 b - -"
    ).foreach: fen =>
      assertEquals(
        toFen(SmallFen.make(Standard, Fen.Simple(fen)).value),
        SimpleFen(fen)
      )
