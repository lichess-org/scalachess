package chess
package bitboard

import chess.format.EpdFen

object FenFixtures:
  val fens = List(
    "2rqkb1r/1b2pppp/p1n2n2/1p1p4/3P1B2/2NBP2P/PP3PP1/2RQK1NR w Kk - 1 10",
    "2rq1rk1/1b2bppp/p1n1pn2/3p4/1p1P1B2/P1NBPN1P/1P2QPP1/2R2RK1 w - - 0 14",
    "8/P7/8/8/4k3/1B5P/3p2PK/5r2 w - - 0 54",
    "1nbqkbnr/pppp1ppp/4p3/8/4K3/4r3/PPPPPPPP/RNBQ1BNR w - - 0 1",
    "r2qk1nr/ppp2ppp/2nb4/3p4/3P2b1/2PB1N2/PP3PPP/RNBQK2R w KQkq - 3 7",
    "r2qk1nr/ppp2ppp/3b4/3p4/3P2b1/2PB1Nn1/PP3PPP/RNBQK2R w KQkq - 3 7",
    "r3k1nr/ppp2ppp/q2b1n2/3p4/3PB1b1/2P2N2/PP3PPP/RNBQK2R w KQkq - 3 7"
  ).map(EpdFen(_))
