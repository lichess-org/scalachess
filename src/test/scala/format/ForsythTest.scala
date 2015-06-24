package chess
package format

import Forsyth.SituationPlus
import Pos._

class ForsythTest extends ChessTest {

  val f = Forsyth

  "the forsyth notation" should {
    "export" in {
      "game opening" in {
        val moves = List(E2 -> E4, C7 -> C5, G1 -> F3, G8 -> H6, A2 -> A3)
        "new game" in {
          f >> makeGame must_== "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        }
        "new game board only" in {
          f exportBoard makeBoard must_== "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        }
        "one move" in {
          makeGame.playMoveList(moves take 1) must beSuccess.like {
            case g => f >> g must_== "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
          }
        }
        "2 moves" in {
          makeGame.playMoveList(moves take 2) must beSuccess.like {
            case g => f >> g must_== "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
          }
        }
        "3 moves" in {
          makeGame.playMoveList(moves take 3) must beSuccess.like {
            case g => f >> g must_== "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
          }
        }
        "4 moves" in {
          makeGame.playMoveList(moves take 4) must beSuccess.like {
            case g => f >> g must_== "rnbqkb1r/pp1ppppp/7n/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
          }
        }
        "5 moves" in {
          makeGame.playMoveList(moves take 5) must beSuccess.like {
            case g => f >> g must_== "rnbqkb1r/pp1ppppp/7n/2p5/4P3/P4N2/1PPP1PPP/RNBQKB1R b KQkq - 0 3"
          }
        }
      }
    }
    "import" in {
      "torus" in {
        "A8 + 1" in { f.tore(A8, 1) must_== Some(B8) }
        "A8 + 2" in { f.tore(A8, 2) must_== Some(C8) }
        "A8 + 7" in { f.tore(A8, 7) must_== Some(H8) }
        "A8 + 8" in { f.tore(A8, 8) must_== Some(A7) }
        "C4 + 3" in { f.tore(C4, 3) must_== Some(F4) }
        "C4 + 8" in { f.tore(C4, 8) must_== Some(C3) }
        "F1 + 2" in { f.tore(F1, 2) must_== Some(H1) }
      }
      val moves = List(E2 -> E4, C7 -> C5, G1 -> F3, G8 -> H6, A2 -> A3)
      def compare(ms: List[(Pos, Pos)], fen: String) =
        makeGame.playMoveList(ms) must beSuccess.like {
          case g => (f << fen) must beSome.like {
            case situation => situation.board.visual must_== g.situation.board.visual
          }
        }
      "new game" in {
        compare(
          Nil,
          "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        )
      }
      "one move" in {
        compare(
          moves take 1,
          "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        )
      }
      "2 moves" in {
        compare(
          moves take 2,
          "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
        )
      }
      "3 moves" in {
        compare(
          moves take 3,
          "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
        )
      }
      "4 moves" in {
        compare(
          moves take 4,
          "rnbqkb1r/pp1ppppp/7n/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
        )
      }
      "5 moves" in {
        compare(
          moves take 5,
          "rnbqkb1r/pp1ppppp/7n/2p5/4P3/P4N2/1PPP1PPP/RNBQKB1R b KQkq - 0 3"
        )
      }
      "invalid" in {
        f << "hahaha" must beNone
      }
      // "weird" in {
      //   f << "raalll3qb1r/p1Q1nk1p/2n3p1/8/8/5N2/PPPP1PPP/RNB1K2R b KQ - 0 10" must beNone
      // }
      // "too long" in {
      //   f << "r3qb1r/p1Q1nk1p/2n3p1/8/8/5N2/PPPP1PPP/RNB1K2R/RNB1K2R b KQ - 0 10" must beNone
      // }
    }
  }
  "export to situation plus" should {
    "with turns" in {
      "starting" in {
        f <<< "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" must beSome.like {
          case s => s.turns must_== 0
        }
      }
      "white to play" in {
        f <<< "r2q1rk1/ppp2pp1/1bnpbn1p/4p3/4P3/1BNPBN1P/PPPQ1PP1/R3K2R w KQ - 7 10" must beSome.like {
          case s => s.turns must_== 18
        }
      }
      "black to play" in {
        f <<< "r1q2rk1/ppp2ppp/3p1n2/8/2PNp3/P1PnP3/2QP1PPP/R1B2K1R b - - 3 12" must beSome.like {
          case s => s.turns must_== 23
        }
      }
      "last move (for en passant)" in {
        f <<< "2b2rk1/3p2pp/2pNp3/4PpN1/qp1P3P/4P1K1/6P1/1Q6 w - f6 0 36" must beSome.like {
          case s => s.situation.board.history.lastMove must_== Some(Pos.F7, Pos.F5)
        }
      }
      "last move (for en passant in Pretrov's defense)" in {
        f <<< "rnbqkb1r/ppp2ppp/8/3pP3/3Qn3/5N2/PPP2PPP/RNB1KB1R w KQkq d6 0 6" must beSome.like {
          case s => s.situation.board.history.lastMove must_== Some(Pos.D7, Pos.D5)
        }
      }
      "last move (for en passant with black to move)" in {
        f <<< "4k3/8/8/8/4pP2/8/2K5/8 b - f3 0 1" must beSome.like {
          case s => s.situation.board.history.lastMove must_== Some(Pos.F2, Pos.F4)
        }
      }
    }
    "with history" in {
      "starting" in {
        f <<< "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" must beSome.like {
          case SituationPlus(Situation(Board(_, History(_, _, Castles(true, true, true, true), _), _), _), _) => ok
        }
      }
      "white to play" in {
        f <<< "r2q1rk1/ppp2pp1/1bnpbn1p/4p3/4P3/1BNPBN1P/PPPQ1PP1/R3K2R w KQ - 7 10" must beSome.like {
          case SituationPlus(Situation(Board(_, History(_, _, Castles(true, true, false, false), _), _), _), _) => ok
        }
      }
      "black to play" in {
        f <<< "r1q2rk1/ppp2ppp/3p1n2/8/2PNp3/P1PnP3/2QP1PPP/R1B2K1R b - - 3 12" must beSome.like {
          case SituationPlus(Situation(Board(_, History(_, _, Castles(false, false, false, false), _), _), _), _) => ok
        }
      }
    }
  }
  "fix impossible castle flags" should {
    "messed up" in {
      val fen = "yayyyyyyyyyyy"
      f fixCastles fen must beNone
    }
    "initial" in {
      val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      f fixCastles fen must beSome(fen)
    }
    "white king out" in {
      val fen = "rnbqkbnr/pppppppp/8/8/4K3/8/PPPPPPPP/RNBQ1BNR w KQkq - 0 1"
      val fix = "rnbqkbnr/pppppppp/8/8/4K3/8/PPPPPPPP/RNBQ1BNR w kq - 0 1"
      f fixCastles fen must beSome(fix)
    }
    "black king out" in {
      val fen = "rnbq1bnr/pppppppp/3k4/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      val fix = "rnbq1bnr/pppppppp/3k4/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 0 1"
      f fixCastles fen must beSome(fix)
    }
    "white king rook out" in {
      val fen = "rnbqkbnr/pppppppp/8/8/8/7R/PPPPPPPP/RNBQKBN1 w KQkq - 0 1"
      val fix = "rnbqkbnr/pppppppp/8/8/8/7R/PPPPPPPP/RNBQKBN1 w Qkq - 0 1"
      f fixCastles fen must beSome(fix)
    }
    "white queen rook out" in {
      val fen = "rnbqkbnr/pppppppp/8/8/8/R7/PPPPPPPP/1NBQKBNR w KQkq - 0 1"
      val fix = "rnbqkbnr/pppppppp/8/8/8/R7/PPPPPPPP/1NBQKBNR w Kkq - 0 1"
      f fixCastles fen must beSome(fix)
    }
    "white king rook out" in {
      val fen = "rnbqkbnr/pppppppp/8/8/8/7R/PPPPPPPP/RNBQKBN1 w KQkq - 0 1"
      val fix = "rnbqkbnr/pppppppp/8/8/8/7R/PPPPPPPP/RNBQKBN1 w Qkq - 0 1"
      f fixCastles fen must beSome(fix)
    }
    "white both rooks out" in {
      val fen = "rnbqkbnr/pppppppp/8/8/8/R6R/PPPPPPPP/1NBQKBN1 w KQkq - 0 1"
      val fix = "rnbqkbnr/pppppppp/8/8/8/R6R/PPPPPPPP/1NBQKBN1 w kq - 0 1"
      f fixCastles fen must beSome(fix)
    }
    "white and black both rooks out" in {
      val fen = "1nbqkbn1/pppppppp/8/8/8/R6R/PPPPPPPP/1NBQKBN1 w KQkq - 0 1"
      val fix = "1nbqkbn1/pppppppp/8/8/8/R6R/PPPPPPPP/1NBQKBN1 w - - 0 1"
      f fixCastles fen must beSome(fix)
    }
  }
  "ignore impossible en passant squares" should {
    "with queen instead of pawn" in {
      f <<< "8/4k3/8/6K1/1pp5/2q5/1P6/8 w - c3 0 1" must beSome.like {
        case s => s.situation.board.history.lastMove isEmpty
      }
    }
    "with no pawn" in {
      f <<< "8/8/8/5k2/5p2/8/5K2/8 b - g3 0 1" must beSome.like {
        case s => s.situation.board.history.lastMove isEmpty
      }
    }
    "with non empty en passant squares" should {
      f <<< "8/8/8/5k2/5pP1/8/6K1/8 b - g3 0 1" must beSome.like {
        case s => s.situation.board.history.lastMove isEmpty
      }
    }
    "with wrong side to move" should {
      f <<< "8/8/8/5k2/5pP1/8/1K6/8 w - g3 0 1" must beSome.like {
        case s => s.situation.board.history.lastMove isEmpty
      }
    }
  }
}
