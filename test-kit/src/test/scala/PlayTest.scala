package chess

import chess.format.Visual.addNewLines
import chess.Square.*
import chess.format.{ EpdFen, Fen }
import chess.variant.Standard

class PlayTest extends ChessSpecs:

  "playing a game" should:
    "preserve castling rights" in:
      "only kingside rights" in:
        val game = fenToGame(
          EpdFen("4k2r/8/8/6R1/6r1/3K4/8/8 b k - 3 4"),
          Standard
        )
        game must beRight.like { game =>
          game.playMoves(
            G4 -> G2,
            G5 -> G8,
            G2 -> G8,
            D3 -> E3,
            G8 -> G5
          ) must beRight.like { game =>
            val fen = Fen write game
            fen must_== "4k2r/8/8/6r1/8/4K3/8/8 w k - 2 3"
          }
        }

      "kingside and queenside rights" in:
        val game = fenToGame(
          EpdFen("r3k2r/8/8/6R1/6r1/3K4/8/8 b kq - 3 4"),
          Standard
        )
        game must beRight.like { game =>
          game.playMoves(
            G4 -> G2,
            G5 -> G8,
            G2 -> G8,
            D3 -> E3,
            G8 -> G5
          ) must beRight.like { game =>
            val fen = Fen write game
            fen must_== "r3k2r/8/8/6r1/8/4K3/8/8 w kq - 2 3"
          }
        }

    "opening one" in:
      val game =
        makeGame.playMoves(E2 -> E4, E7 -> E5, F1 -> C4, G8 -> F6, D2 -> D3, C7 -> C6, C1 -> G5, H7 -> H6)
      "current game" in:
        game must beRight.like { case g =>
          addNewLines(g.board.visual) must_== """
rnbqkb r
pp p pp
  p  n p
    p B
  B P
   P
PPP  PPP
RN QK NR
"""
        }
      "after recapture" in:
        game flatMap { _.playMoves(G5 -> F6, D8 -> F6) } must beRight.like { case g =>
          addNewLines(g.board.visual) must_== """
rnb kb r
pp p pp
  p  q p
    p
  B P
   P
PPP  PPP
RN QK NR
"""
        }
    "Deep Blue vs Kasparov 1" in:
      makeGame.playMoves(
        E2 -> E4,
        C7 -> C5,
        C2 -> C3,
        D7 -> D5,
        E4 -> D5,
        D8 -> D5,
        D2 -> D4,
        G8 -> F6,
        G1 -> F3,
        C8 -> G4,
        F1 -> E2,
        E7 -> E6,
        H2 -> H3,
        G4 -> H5,
        E1 -> G1,
        B8 -> C6,
        C1 -> E3,
        C5 -> D4,
        C3 -> D4,
        F8 -> B4
      ) must beRight.like { case g =>
        addNewLines(g.board.visual) must_== """
r   k  r
pp   ppp
  n pn
   q   b
 b P
    BN P
PP  BPP
RN Q RK
"""
      }
    "Peruvian Immortal" in:
      makeGame.playMoves(
        E2 -> E4,
        D7 -> D5,
        E4 -> D5,
        D8 -> D5,
        B1 -> C3,
        D5 -> A5,
        D2 -> D4,
        C7 -> C6,
        G1 -> F3,
        C8 -> G4,
        C1 -> F4,
        E7 -> E6,
        H2 -> H3,
        G4 -> F3,
        D1 -> F3,
        F8 -> B4,
        F1 -> E2,
        B8 -> D7,
        A2 -> A3,
        E8 -> C8,
        A3 -> B4,
        A5 -> A1,
        E1 -> D2,
        A1 -> H1,
        F3 -> C6,
        B7 -> C6,
        E2 -> A6
      ) must beRight.like { case g =>
        addNewLines(g.board.visual) must_== """
  kr  nr
p  n ppp
B p p

 P P B
  N    P
 PPK PP
       q
"""
      }
