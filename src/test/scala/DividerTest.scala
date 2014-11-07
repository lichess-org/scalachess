package chess

class DividerTest extends ChessTest {

  def makeReplay(moves: String) = format.pgn.Reader(moves).err

  "the divider finds middlegame and endgame" should {

    "game1" in {
      val replay = makeReplay("e3 Nc6 d4 Nf6 c3 e5 dxe5 Nxe5 Bb5 a6 Ba4 b5 Bb3 d5 e4 dxe4 f4 Qxd1+ Kxd1 Nd3 Be3 Ng4 Bd4 Ngf2+ Bxf2 Nxf2+ Ke1 Nxh1 Bd5 Ra7 Bc6+ Kd8 Bxe4 Bd6 g3 Re8 Nd2 f5 Ne2 fxe4 Kf1 e3 Kg2 exd2 Rxh1 Bb7+ Kf2 Bc5+ Kf1 d1=Q#")
      Divider(replay) === (Option(12), Option(25))
    }
    "game2" in {
      val replay = makeReplay("c4 Nc6 e3 Nf6 h3 Ne4 d3 Nc5 a3 Ne5 d4 d6 dxe5 dxe5 b4 Qxd1+ Kxd1 Ne4 f3 Nf2+ Ke2 Nxh1 Nd2 Ng3+ Ke1 Bf5 Bd3 Bxd3 Rb1 Bxb1 Nxb1 Rd8 Bd2 e6 h4 Be7 Nh3 Bxh4 Nf2 Ke7 Bc3 f6 Nd2 h5 c5 g5 Nc4 Rhg8 Na5 Nh1 Ke2 Nxf2 Be1 Nd3 Nxb7 Bxe1 Nxd8 Rxd8 c6 a5 bxa5 Bxa5 a4 f5 Kd1 Nf4+ Kc2 Rd2+ Kc1 Nxg2 Kb1 Nxe3 Kc1 h4 Kb1 h3 Kc1 h2 Kb1 h1=Q#")
      Divider(replay) === (Option(6), Option(32))
    }
    "game3" in {
      val replay = makeReplay("e4 c5 Nf3 d6 d4 cxd4 Nxd4 Nc6 Nc3 e5 Nb3 Nf6 f3 Be7 Be3 O-O Qd2 b6 O-O-O Bb7 g4 Rc8 h4 a5 h5 Nb4 g5 Nd7 g6 Nc5 h6 Nxb3+ axb3 fxg6 hxg7 Rxf3 Bxb6 Qxb6 Qh6 Qe3+ Qxe3 Rxe3 Bd3 Nxd3+ cxd3 Bg5 Kb1 Kxg7 Rh2 Bf4 Rh4 h5 Rhh1 Rf8 Rhg1 g5 Rg2 g4 Nb5 Rf6 Nc7 Rg3 Rxg3 Bxg3 Ne8+ Kf7 Nxf6 Kxf6 Rf1+ Kg5 Rf7 Ba6 Ra7 Bxd3+ Kc1 Bf4+ Kd1 Bxe4 Rxa5 g3 Ke2 g2 Ra1 h4 Kf2 h3 b4 Kg4 b5 h2 b6 h1=Q Rg1 Be3+ Kxe3 Qxg1+ Kxe4 Qxb6 Kd5 g1=Q Ke6 Qgf2 Kd5 Qf3+ Ke6 Qbb3+ Ke7 Qff7+ Kxd6 Qbd5#")
      Divider(replay) === (Option(21), Option(69))
    }
  }
}
