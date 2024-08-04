package chess
package motif

// https://www.chessprogramming.org/Forsyth-Edwards_Notation#Fullmove_counter
// ply FEN (ends with full move number)
// 0   rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
// 1   rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
// 2   rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2
// 3   rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

class MotifTest extends ChessTest:

  test("to full move number"):
    assertEquals(Ply(0).fullMoveNumber, FullMoveNumber(1)) // root

    def test_fork(self):
        self.assertTrue(cook.fork(make("0PQep", "6q1/p6p/6p1/4k3/1P2N3/2B2P2/4K1P1/8 b - - 3 43", "e5d5 e4f6 d5c4 f6g8")))
        self.assertFalse(cook.fork(make("0O5RW", "rnb1k2r/p1B2ppp/4p3/1Bb5/8/4P3/PP1K1PPP/nN4NR b kq - 0 12", "b8d7 b5c6 c8a6 c6a8 c5b4 b1c3")))
        self.assertTrue(cook.fork(make("1NxIN", "r3k2r/p2q1ppp/4pn2/1Qp5/8/4P3/PP1N1PPP/R3K2R w KQkq - 2 16", "b5c5 d7d2 e1d2 f6e4 d2e2 e4c5")))
        self.assertFalse(cook.fork(make("6ppA2", "8/p7/1p6/2p5/P6P/2P2Nk1/1r4P1/4R1K1 w - - 1 39", "f3d2 b2d2 h4h5 d2g2")))
        self.assertFalse(cook.fork(make("bypCs", "rnbq1b1r/p1k1pQp1/2p4p/1p1nP1p1/2pP4/2N3B1/PP3P1P/R3KBNR w KQ - 5 14", "c3d5 d8d5 f7d5 c6d5")))
        self.assertFalse(cook.fork(make("qgSLr", "2r3k1/6p1/p2q1rRp/3pp3/3P1p1R/3Q3P/PP3PP1/6K1 w - - 0 31", "g6f6 d6f6 h4h5 e5e4 d3b3 g7g5 b3d5 f6f7 d5e4 c8c1 g1h2 f7h5")))
        self.assertFalse(cook.fork(make("2eqdQ", "r4rk1/pp2qppp/5p2/1b1p4/1b1Q4/2N1B3/PPP2PPP/2KR3R b - - 7 13", "b4c5 d4c5 e7c5 e3c5")))
        self.assertFalse(cook.fork(make("QNrtc", "r2qr1k1/5p1p/pn3bp1/1p6/3P2bN/1P1B2PP/PB3PQ1/R3R1K1 b - - 0 19", "f6d4 e1e8 d8e8 b2d4")))
        self.assertFalse(cook.fork(make("J72FN", "6k1/7p/3R2p1/8/5p2/P4P2/1P1N2PP/3r1nK1 w - - 0 33", "d2e4 f1d2 g1f2 d2e4")))
