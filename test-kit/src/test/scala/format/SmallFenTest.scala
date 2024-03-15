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

  test("standard, skip w, b6 en passant"):
    assertEquals(
      SmallFen
        .make(Standard, Fen.Simple("8/8/p1k2nB1/Pp1np3/1PK4P/8/3B4/8 w - b6"))
        .garbageInefficientReadBackIntoFen,
      SimpleFen("8/8/p1k2nB1/Pp1np3/1PK4P/8/3B4/8 w - b6")
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
      "8/8/2k5/5q2/5n2/8/5K2/8 b - -",
      "k7/8/1K6/3BB3/8/8/8/8 b - -",
      "8/8/8/4b3/4b3/1k6/8/K7 w - -",
      "k7/8/1K6/3BB3/8/8/8/5q2 b - -",
      "8/1P6/8/4b3/4b3/1k6/8/K7 w - -",
      "k7/8/1KBB4/8/8/8/6P1/8 b - -",
      "K7/8/1kbb4/8/8/8/6p1/8 w - -",
      "8/8/8/6K1/4NN2/3NN3/8/4k3 w - -",
      "8/8/8/6k1/4nn2/3nn3/8/4K3 w - -",
      "k7/8/1NN5/1NN5/8/8/8/K7 w - -",
      "K7/8/1nn5/1nn5/8/8/8/k7 w - -",
      "k7/8/1NN5/1NN5/8/8/4Q3/K7 w - -",
      "3k4/8/2NNN3/2NNN3/8/8/8/3K4 w - -",
      "3K4/8/2nnn3/2nnn3/8/8/8/3k4 w - -",
      "8/8/4NN2/4NN1k/4NN2/8/8/3K4 w - -",
      "8/8/4nn2/4nn1K/4nn2/8/8/3k4 w - -",
      "3k4/8/2NNN3/2NNN3/8/3Q4/8/3K4 w - -",
      "8/8/8/3K4/1NNNNN2/3Nk3/8/8 b - -",
      "k7/3N4/1KB5/8/8/8/8/8 b - -",
      "K7/3n4/1kb5/8/8/8/8/8 w - -",
      "k7/3N4/1KB5/8/8/6P1/8/8 w - -",
      "8/8/8/8/4kn2/4Bp2/6q1/4K3 b - -",
      "8/7P/8/8/4K3/8/1bk5/8 w - -",
      "3rkb1r/pbN2qpp/np1B4/2p1N3/Q3n3/4P3/PP3PPP/R3K2R b KQk -",
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -",
      "rnbqkbnr/ppppp1pp/8/5p1Q/8/4P3/PPPP1PPP/RNB1KBNR b KQkq -",
      "8/8/8/1nn5/1nn5/1nn2k2/8/2K5 w - -",
      "2bqkb1r/2p2p1p/p1n2n1p/p2pp3/3P4/P3PN2/1PP2PP1/RNBQK2R w KQk -",
      "8/8/3p3k/1PpP3P/P5KP/P7/8/8 b - -",
      "2kr4/1p3p2/p1n1p1p1/3p1p2/PP1PpP1r/N1P1P2P/6P1/2K2R1R w - -",
      "7k/7P/5PK1/8/8/6P1/8/8 b - -",
      "3K4/q7/2q5/3k4/8/8/8/8 w - -",
      "5K2/6r1/6q1/8/8/8/4k3/8 w - -",
      "8/8/8/8/5P2/3QK3/8/4k3 b - -",
      "8/1kbK4/8/8/8/8/8/4q3 w - -",
      "8/8/8/6k1/8/qp6/8/1K6 w - -",
      "8/8/8/5K2/k7/2B5/1Q6/8 b - -",
      "8/8/8/8/2N1K3/8/2Q5/k7 b - -",
      "7k/5P2/6KP/8/8/8/8/8 b - -",
      "3K4/8/3kq3/8/8/8/8/8 w - -",
      "8/3b4/8/8/p7/Kpk5/8/8 w - -",
      "7k/7P/7K/8/8/8/8/8 b - -",
      "k7/2K5/8/2B5/8/p7/P7/8 b - -",
      "1k6/1P6/1K6/2P5/8/8/8/8 b - -",
      "8/8/5R2/8/8/8/7p/5K1k b - -",
      "8/8/8/8/8/5p2/5K1p/7k b - -"
    ).foreach: fen =>
      assertEquals(
        toFen(SmallFen.make(Standard, Fen.Simple(fen)).value),
        SimpleFen(fen)
      )
