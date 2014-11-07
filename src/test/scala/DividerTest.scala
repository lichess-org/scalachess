package chess

class DividerTest extends ChessTest {

  def makeReplay(moves: String) = format.pgn.Reader(moves).err

  "the divider finds middlegame and endgame" should {

    "game1" in {
      val replay = makeReplay("e3 Nc6 d4 Nf6 c3 e5 dxe5 Nxe5 Bb5 a6 Ba4 b5 Bb3 d5 e4 dxe4 f4 Qxd1+ Kxd1 Nd3 Be3 Ng4 Bd4 Ngf2+ Bxf2 Nxf2+ Ke1 Nxh1 Bd5 Ra7 Bc6+ Kd8 Bxe4 Bd6 g3 Re8 Nd2 f5 Ne2 fxe4 Kf1 e3 Kg2 exd2 Rxh1 Bb7+ Kf2 Bc5+ Kf1 d1=Q#")
      val divided = Divider(replay) 
      divided._1 must beNone
      divided._2 must beSome.like {
        case x => x must beBetween(43, 46)
      }
    }
    "game2" in {
      val replay = makeReplay("c4 Nc6 e3 Nf6 h3 Ne4 d3 Nc5 a3 Ne5 d4 d6 dxe5 dxe5 b4 Qxd1+ Kxd1 Ne4 f3 Nf2+ Ke2 Nxh1 Nd2 Ng3+ Ke1 Bf5 Bd3 Bxd3 Rb1 Bxb1 Nxb1 Rd8 Bd2 e6 h4 Be7 Nh3 Bxh4 Nf2 Ke7 Bc3 f6 Nd2 h5 c5 g5 Nc4 Rhg8 Na5 Nh1 Ke2 Nxf2 Be1 Nd3 Nxb7 Bxe1 Nxd8 Rxd8 c6 a5 bxa5 Bxa5 a4 f5 Kd1 Nf4+ Kc2 Rd2+ Kc1 Nxg2 Kb1 Nxe3 Kc1 h4 Kb1 h3 Kc1 h2 Kb1 h1=Q#")
      val divided = Divider(replay)
      divided._1 must beSome.like {
        case x => x must beBetween(45, 50)
      }
      divided._2 must beSome.like {
        case x => x must beBetween(30, 35)
      }
    }
    "game3" in {
      val replay = makeReplay("e4 c5 Nf3 d6 d4 cxd4 Nxd4 Nc6 Nc3 e5 Nb3 Nf6 f3 Be7 Be3 O-O Qd2 b6 O-O-O Bb7 g4 Rc8 h4 a5 h5 Nb4 g5 Nd7 g6 Nc5 h6 Nxb3+ axb3 fxg6 hxg7 Rxf3 Bxb6 Qxb6 Qh6 Qe3+ Qxe3 Rxe3 Bd3 Nxd3+ cxd3 Bg5 Kb1 Kxg7 Rh2 Bf4 Rh4 h5 Rhh1 Rf8 Rhg1 g5 Rg2 g4 Nb5 Rf6 Nc7 Rg3 Rxg3 Bxg3 Ne8+ Kf7 Nxf6 Kxf6 Rf1+ Kg5 Rf7 Ba6 Ra7 Bxd3+ Kc1 Bf4+ Kd1 Bxe4 Rxa5 g3 Ke2 g2 Ra1 h4 Kf2 h3 b4 Kg4 b5 h2 b6 h1=Q Rg1 Be3+ Kxe3 Qxg1+ Kxe4 Qxb6 Kd5 g1=Q Ke6 Qgf2 Kd5 Qf3+ Ke6 Qbb3+ Ke7 Qff7+ Kxd6 Qbd5#")
      val divided = Divider(replay)
      divided._1 must beSome.like {
        case x => x must beBetween(23, 28)
      }
      divided._2 must beSome.like {
        case x => x must beBetween(43, 48)
      }
    }
    "game4" in {
      val replay = makeReplay("e4 c5 f4 Nc6 Nf3 d6 Bb5 Bd7 O-O g6 d3 Bg7 c4 Nf6 Nc3 O-O Qe1 a6 Bxc6 Bxc6 Bd2 Rb8 b3 b5 Rd1 bxc4 dxc4 e6 e5 Ne8 exd6 Nxd6 Bc1 Qc7 Ne5 Ba8 Qe2 Rfd8 Be3 Nf5 Na4 Bxe5 fxe5 Qxe5 Rde1 Nd4 Bxd4 Qxd4+ Kh1 Bc6 Rd1 Qh4 Nxc5 Rxd1 Rxd1 Qg5 Ne4 Bxe4 Qxe4 Qh5 Qd3 Kg7 Qd4+ f6 Qd7+ Kh6 Re1 Rd8 Qxe6 Rd1 Kg1 Rd2 Qe3+ Qg5 Qxg5+ fxg5 Ra1 g4 a3 Kg5 b4 Rd4 b5 axb5 cxb5 h5 a4 Rb4 g3 h4 a5 Rxb5 a6 Rb8 a7 Ra8 Kf2 hxg3+ Kxg3 Kf5 Ra6 g5 Ra5+ Kf6 Kxg4 Ke6 Kxg5 Kd6 h4 Kc6 h5 Kb6 Ra1 Kb7 h6 Rg8+ Kf6 Ra8 h7 Kb6 Kg7 Rd8 h8=Q Rd7+ Kf6 Rd6+ Ke5 Rc6 Qb8+ Kc5 Rc1#")
      val divided = Divider(replay)
      divided._1 must beSome.like {
        case x => x must beBetween(10, 15)
      }
      divided._2 must beSome.like {
        case x => x must beBetween(55, 65)
      }
    }
    "game5" in {
      val replay = makeReplay("e4 c5 Nf3 d6 Bc4 Nf6 d3 g6 c3 Bg7 Bg5 O-O h3 Nc6 Nbd2 a6 Bb3 b5 Bc2 Bb7 O-O Nd7 Nh2 f6 Be3 e5 Ndf3 Ne7 Qd2 f5 Qe2 h6 Bd2 g5 g4 f4 Bb3+ d5 exd5 Bxd5 c4 Bf7 cxb5 axb5 Bxf7+ Rxf7 Bc3 Ng6 a3 b4 axb4 Rxa1 Rxa1 cxb4 Bxb4 Qb6 Bc3 Re7 Ra8+ Kh7 Kg2 Nc5 Qc2 Qb3 Qxb3 Nxb3 Rb8 Nc5 Rb5 Nxd3 Nf1 e4 Ng1 Nh4+ Kh2 Bxc3 bxc3 Nxf2 Rd5 e3 Ne2 Nf3+ Kg2 Ne1+ Kh2 f3 Neg3 e2 Nd2 Ng2 Nxf3 e1=Q Nxe1 Nxe1 c4 Nc2 c5 Ne4 c6 Nxg3 Kxg3 Re3+ Kf2 Rc3 Rd7+ Kg6 c7 Nb4 Rd6+ Kf7 Rxh6 Rxc7 Rh7+ Ke6 Rxc7 Nd3+ Kf3 Nf4 Kg3 Ne2+ Kf2 Nf4 Kg3 Ne2+ Kh2 Nf4 Rc5 Kf6 Rf5+ Kg6 Kg3 Ne2+ Kf3 Nd4+ Ke4 Nxf5 gxf5+ Kf6 h4 gxh4 Kf4 h3 Kg3 Kxf5 Kxh3")
      val divided = Divider(replay)
      divided._1 must beSome.like {
        case x => x must beBetween(15, 20)
      }
      divided._2 must beSome.like {
        case x => x must beBetween(65, 70)
      }
    }
    "game6" in {
      val replay = makeReplay("c4 e6 Nc3 d5 cxd5 exd5 d4 Nf6 g3 c5 e3 Nc6 Nf3 Bg4 Bg2 Be7 O-O O-O h3 Bh5 b3 cxd4 exd4 Re8 Bf4 a6 Rc1 Qb6 g4 Bg6 Ne5 Nxd4 Bxd5 Ba3 Nc4 Qc5 Bd6 Ne2+ Qxe2 Qd4 Qf3 Bxc1 Rxc1 Nxd5 Nxd5 Be4 Qe3 Qxd5 Nb6 Qxd6 Nxa8 Bc6 Rxc6 Qd1+ Kg2 Qd5+ f3 Qxc6 Qa7 Qc2+ Kg3 Qe2 Qxb7 Qe5+ Kf2 Qd4+ Kg3 Qg1+ Kh4 Qf2+ Kg5 f6+ Kf4 Qh2+ Kf5 Re5#")
      val divided = Divider(replay)
      divided._1 must beSome.like {
        case x => x must beBetween(20, 25)
      }
      divided._2 must beSome.like {
        case x => x must beBetween(55, 60)
      }
    }
  }
}
