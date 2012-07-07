package chess
package format.pgn

import Pos._

class DumperTest extends ChessTest {

  val gioachineGreco = makeGame.playMoves(D2 -> D4, D7 -> D5, C2 -> C4, D5 -> C4, E2 -> E3, B7 -> B5, A2 -> A4, C7 -> C6, A4 -> B5, C6 -> B5, D1 -> F3)

  val peruvianImmortal = makeGame.playMoves(E2 -> E4, D7 -> D5, E4 -> D5, D8 -> D5, B1 -> C3, D5 -> A5, D2 -> D4, C7 -> C6, G1 -> F3, C8 -> G4, C1 -> F4, E7 -> E6, H2 -> H3, G4 -> F3, D1 -> F3, F8 -> B4, F1 -> E2, B8 -> D7, A2 -> A3, E8 -> C8, A3 -> B4, A5 -> A1, E1 -> D2, A1 -> H1, F3 -> C6, B7 -> C6, E2 -> A6)

  "standard game" should {
    "move list" in {
      "Gioachine Greco" in {
        gioachineGreco map (_.pgnMoves) must beSuccess.like {
          case ms ⇒ ms must_== "d4 d5 c4 dxc4 e3 b5 a4 c6 axb5 cxb5 Qf3"
        }
      }
      "Peruvian Immortal" in {
        peruvianImmortal map (_.pgnMoves) must beSuccess.like {
          case ms ⇒ ms must_== "e4 d5 exd5 Qxd5 Nc3 Qa5 d4 c6 Nf3 Bg4 Bf4 e6 h3 Bxf3 Qxf3 Bb4 Be2 Nd7 a3 O-O-O axb4 Qxa1+ Kd2 Qxh1 Qxc6+ bxc6 Ba6#"
        }
      }
    }
  }
  "dump a promotion move" should {
    "without check" in {
      val game = Game("""
   
P    k




PP   PPP
KNBQ BNR
""")
      game.playMoves(A7 -> A8) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "a8=Q"
      }
    }
    "with check" in {
      val game = Game("""
    k
P     




PP   PPP
KNBQ BNR
""")
      game.playMoves(A7 -> A8) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "a8=Q+"
      }
    }
    "with checkmate" in {
      val game = Game("""
    k
P  ppp  




PP   PPP
KNBQ BNR
""")
      game.playMoves(A7 -> A8) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "a8=Q#"
      }
    }
    "castle kingside" in {
      Game("""
PP   PPP
R   K  R
""").playMoves(E1 -> G1) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "O-O"
      }
    }
    "castle queenside" in {
      Game("""
PP   PPP
R   K  R
""").playMoves(E1 -> C1) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "O-O-O"
      }
    }
  }
  "chess960" should {
    "castle queenside as white" in {
      Game(makeBoard("""
PPPPPPPP
NRK RQBB
""", Variant.Chess960)).playMoves(C1 -> B1) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "O-O-O"
      }
    }
    "castle kingside as white" in {
      Game(makeBoard("""
PP PPPPP
NRK R  B
""", Variant.Chess960)).playMoves(C1 -> E1) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "O-O"
      }
    }
    "castle queenside as black" in {
      Game(makeBoard("""
nrk rqbb
pppppppp




PPPPPPPP
NRK RQBB
""", Variant.Chess960)).withPlayer(Black).playMoves(C8 -> B8) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "O-O-O"
      }
    }
    "castle kingside as black" in {
      Game(makeBoard("""
nrk r  b
pppppppp




PPPPPPPP
NRK RQBB
""", Variant.Chess960)).withPlayer(Black).playMoves(C8 -> E8) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "O-O"
      }
    }
    "opening with castles" in {
      Game(makeBoard("""
nrknrqbb
pppppppp




PPPPPPPP
NRKNRQBB
""", Variant.Chess960)).playMoves(
  F2 -> F4,
  D8 -> C6,
  D1 -> C3,
  G7 -> G6,
  C3 -> B5,
  C8 -> B8,
  C1 -> B1
) map (_.pgnMoves) must beSuccess.like {
        case ms ⇒ ms must_== "f4 Nc6 Nc3 g6 Nb5 O-O-O O-O-O"
      }
    }
  }
}
