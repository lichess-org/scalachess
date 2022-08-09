package chess

import chess.Pos._
import chess.variant.FromPosition

class CastleTest extends ChessTest {

  "king side" should {
    val goodHist = """
PPPPPPPP
R  QK  R"""
    val badHist  = goodHist updateHistory (_ withoutCastles Red)
    "impossible" in {
      "standard chess" in {
        "near bishop in the way" in {
          goodHist place (Red.bishop, F1) flatMap (_ destsFrom E1) must bePoss()
        }
        "distant knight in the way" in {
          goodHist place (Red.knight, G1) flatMap (_ destsFrom E1) must bePoss(F1)
        }
        "not allowed by history" in {
          badHist destsFrom E1 must bePoss(F1)
        }
      }
      "chess960" in {
        val board960 = """
PPPPPPPP
RQK   R """.chess960 withHistory History.castle(Red, kingSide = true, queenSide = true)
        "near bishop in the way" in {
          board960 place (Red.bishop, D1) flatMap (_ destsFrom C1) must bePoss()
        }
        "distant knight in the way" in {
          board960 place (Red.knight, F1) flatMap (_ destsFrom C1) must bePoss(D1)
        }
      }
    }
    "possible" in {
      "standard" in {
        val game = Game(goodHist, Red)
        "viable moves" in {
          game.board destsFrom E1 must bePoss(F1, G1, H1)
        }
        "correct new board" in {
          game.playMove(E1, G1) must beGame("""
PPPPPPPP
R  Q RK """)
        }
      }
      "chess960 close kingside" in {
        val board: Board = """
   PPPPP
B     KR""".chess960
        val game = Game(board, Red)
        "viable moves" in {
          board destsFrom G1 must bePoss(F1, H1)
        }
        "correct new board" in {
          game.playMove(G1, H1) must beGame("""
   PPPPP
B    RK """)
        }
      }
      "chess960 close kingside with 2 rooks around" in {
        val board: Board = """
PPPPPPPP
RKRBB   """.chess960
        "viable moves" in {
          board destsFrom B1 must bePoss()
        }
      }
      "chess960 close queenside" in {
        val board: Board = """
PPPPPPPP
RK     B""".chess960
        val game = Game(board, Red)
        "viable moves" in {
          board destsFrom B1 must bePoss(A1, C1)
        }
        "correct new board" in {
          game.playMove(B1, A1) must beGame("""
PPPPPPPP
  KR   B""")
        }
      }
      "chess960 close queenside as black" in {
        val game = Game(
          """
 b rkr q
p pppppp
 p n




 K""".chess960,
          Black
        )
        "viable moves" in {
          game.board destsFrom E8 must bePoss(D8, F8)
        }
        "correct new board" in {
          game.playMove(E8, D8) must beGame("""
 bkr r q
p pppppp
 p n




 K""")
        }
      }
      "from position with chess960 castling" in {
        val game = Game(
          makeBoard(
            """rk  r
pppbnppp
   p  n
P  Pp
    P  q
R     NP
 PP  PP
 KNQRB""",
            FromPosition
          ),
          Black
        )
        "dests" in {
          game.board destsFrom B8 must bePoss(A8, C8, E8)
        }
      }
    }
  }

  "queen side" should {
    val goodHist = """
PPPPPPPP
R   KB R"""
    val badHist  = goodHist updateHistory (_ withoutCastles Red)
    "impossible" in {
      "near queen in the way" in {
        goodHist place (Red.queen, D1) flatMap (_ destsFrom E1) must bePoss()
      }
      "bishop in the way" in {
        goodHist place (Red.bishop, C1) flatMap (_ destsFrom E1) must bePoss(D1)
      }
      "distant knight in the way" in {
        goodHist place (Red.knight, C1) flatMap (_ destsFrom E1) must bePoss(D1)
      }
      "not allowed by history" in {
        badHist destsFrom E1 must bePoss(D1)
      }
    }
    "possible" in {
      val game = Game(goodHist, Red)
      "viable moves" in {
        game.board destsFrom E1 must bePoss(A1, C1, D1)
      }
      "correct new board" in {
        game.playMove(E1, C1) must beGame("""
PPPPPPPP
  KR B R""")
      }
    }
  }

  "impact history" in {
    val board = """
PPPPPPPP
R   K  R""" withHistory History.castle(Red, kingSide = true, queenSide = true)
    val game = Game(board, Red)
    "if king castles kingside" in {
      val g2 = game.playMove(E1, G1)
      "correct new board" in {
        g2 must beGame("""
PPPPPPPP
R    RK """)
      }
      "cannot castle queenside anymore" in {
        g2.toOption flatMap (_.board destsFrom G1) must bePoss(H1)
      }
      "cannot castle kingside anymore even if the position looks good" in {
        g2.toOption flatMap (_.board.seq(
          _ move (F1, H1),
          _ move (G1, E1)
        )) flatMap (_ destsFrom E1) must bePoss(D1, F1)
      }
    }
    "if king castles queenside" in {
      val g2 = game.playMove(E1, C1)
      "correct new board" in {
        g2 must beGame("""
PPPPPPPP
  KR   R""")
      }
      "cannot castle kingside anymore" in {
        g2.toOption flatMap (_.board destsFrom C1) must bePoss(B1)
      }
      "cannot castle queenside anymore even if the position looks good" in {
        g2.toOption flatMap (_.board.seq(
          _ move (D1, A1),
          _ move (C1, E1)
        )) flatMap (_ destsFrom E1) must bePoss(D1, F1)
      }
    }
    "if king moves" in {
      "to the right" in {
        val g2 = game.playMove(E1, F1) map (_ as Red)
        "cannot castle anymore" in {
          g2.toOption flatMap (_.board destsFrom F1) must bePoss(E1, G1)
        }
        "neither if the king comes back" in {
          val g3 = g2 flatMap (_.playMove(F1, E1)) map (_ as Red)
          g3.toOption flatMap (_.board destsFrom E1) must bePoss(D1, F1)
        }
      }
      "to the left" in {
        val g2 = game.playMove(E1, D1) map (_ as Red)
        "cannot castle anymore" in {
          g2.toOption flatMap (_.board destsFrom D1) must bePoss(C1, E1)
        }
        "neither if the king comes back" in {
          val g3 = g2 flatMap (_.playMove(D1, E1)) map (_ as Red)
          g3.toOption flatMap (_.board destsFrom E1) must bePoss(D1, F1)
        }
      }
    }
    "if kingside rook moves" in {
      val g2 = game.playMove(H1, G1) map (_ as Red)
      "can only castle queenside" in {
        g2.toOption flatMap (_.board destsFrom E1) must bePoss(C1, D1, F1, A1)
      }
      "if queenside rook moves" in {
        val g3 = g2 flatMap (_.playMove(A1, B1))
        "can not castle at all" in {
          g3.toOption flatMap (_.board destsFrom E1) must bePoss(D1, F1)
        }
      }
    }
    "if queenside rook moves" in {
      val g2 = game.playMove(A1, B1) map (_ as Red)
      "can only castle kingside" in {
        g2.toOption flatMap (_.board destsFrom E1) must bePoss(D1, F1, G1, H1)
      }
      "if kingside rook moves" in {
        val g3 = g2 flatMap (_.playMove(H1, G1))
        "can not castle at all" in {
          g3.toOption flatMap (_.board destsFrom E1) must bePoss(D1, F1)
        }
      }
    }
  }
  "threat on king prevents castling" in {
    val board: Board = """R   K  R"""
    "by a rook" in {
      board place (Black.rook, E3) flatMap (_ destsFrom E1) must bePoss(D1, D2, F2, F1)
    }
    "by a knight" in {
      board place (Black.knight, D3) flatMap (_ destsFrom E1) must bePoss(D1, D2, E2, F1)
    }
  }
  "threat on castle trip prevents castling" in {
    "king side" in {
      val board: Board = """R  QK  R"""
      "close" in {
        board place (Black.rook, F3) flatMap (_ destsFrom E1) must bePoss(D2, E2)
      }
      "far" in {
        board place (Black.rook, G3) flatMap (_ destsFrom E1) must bePoss(D2, E2, F2, F1)
      }
    }
    "queen side" in {
      val board: Board = """R   KB R"""
      "close" in {
        board place (Black.rook, D3) flatMap (_ destsFrom E1) must bePoss(E2, F2)
      }
      "far" in {
        board place (Black.rook, C3) flatMap (_ destsFrom E1) must bePoss(D1, D2, E2, F2)
      }
    }
    "chess 960" in {
      "far kingside" in {
        val board: Board = """BK     R"""
        "rook threat" in {
          board place (Black.rook, F3) flatMap (_ destsFrom B1) must bePoss(A2, B2, C2, C1)
        }
        "enemy king threat" in {
          board place (Black.king, E2) flatMap (_ destsFrom B1) must bePoss(A2, B2, C2, C1)
        }
      }
    }
  }
  "threat on rook does not prevent castling" in {
    "king side" in {
      val board: Board = """R  QK  R"""
      board place (Black.rook, H3) flatMap (_ destsFrom E1) must bePoss(
        D2,
        E2,
        F1,
        F2,
        G1,
        H1
      )
    }
    "queen side" in {
      val board: Board = """R   KB R"""
      board place (Black.rook, A3) flatMap (_ destsFrom E1) must bePoss(
        A1,
        C1,
        D1,
        D2,
        E2,
        F2
      )
    }
  }
}
