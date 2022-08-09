package chess
package format

import Forsyth.SituationPlus
import Pos._
import variant._

class ForsythTest extends ChessTest {

  val f = Forsyth

  "the forsyth notation" should {
    "export" in {
      "game opening" in {
        val moves = List(E2 -> E4, C7 -> C5, G1 -> F3, G8 -> H6, A2 -> A3)
        "new game" in {
          f >> makeGame must_== FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR r KQkq - 0 1")
        }
        "new game board only" in {
          f exportBoard makeBoard must_== "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        }
        "one move" in {
          makeGame.playMoveList(moves take 1) must beValid.like { g =>
            f >> g must_== FEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
          }
        }
        "2 moves" in {
          makeGame.playMoveList(moves take 2) must beValid.like { g =>
            f >> g must_== FEN("rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR r KQkq - 0 2")
          }
        }
        "3 moves" in {
          makeGame.playMoveList(moves take 3) must beValid.like { g =>
            f >> g must_== FEN("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
          }
        }
        "4 moves" in {
          makeGame.playMoveList(moves take 4) must beValid.like { g =>
            f >> g must_== FEN("rnbqkb1r/pp1ppppp/7n/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R r KQkq - 2 3")
          }
        }
        "5 moves" in {
          makeGame.playMoveList(moves take 5) must beValid.like { g =>
            f >> g must_== FEN("rnbqkb1r/pp1ppppp/7n/2p5/4P3/P4N2/1PPP1PPP/RNBQKB1R b KQkq - 0 3")
          }
        }
      }

      "completely legal en-passant" in {
        makeGame.playMoves(A2 -> A4, G8 -> F6, A4 -> A5, B7 -> B5) must beValid.like { g =>
          f >> g must_== FEN("rnbqkb1r/p1pppppp/5n2/Pp6/8/8/1PPPPPPP/RNBQKBNR r KQkq b6 0 3")
        }
      }

      "standard castling rights" in {
        val moves = List(
          H2 -> H4,
          H7 -> H5,
          H1 -> H3,
          H8 -> H6,
          B2 -> B4,
          B7 -> B5,
          H3 -> B3,
          H6 -> B6,
          B1 -> A3,
          B8 -> A6,
          B3 -> B1,
          B6 -> B8,
          B1 -> B3,
          B8 -> B6
        )

        "inner rook" in {
          makeGame.playMoveList(moves dropRight 2) must beValid.like { g =>
            f >> g must_== FEN("rrbqkbn1/p1ppppp1/n7/1p5p/1P5P/N7/P1PPPPP1/RRBQKBN1 r Qq - 6 7")
          }
        }

        "inner rook removed" in {
          makeGame.playMoveList(moves) must beValid.like { g =>
            f >> g must_== FEN("r1bqkbn1/p1ppppp1/nr6/1p5p/1P5P/NR6/P1PPPPP1/R1BQKBN1 r Qq - 8 8")
          }
        }
      }
    }
    "import" in {
      val moves = List(E2 -> E4, C7 -> C5, G1 -> F3, G8 -> H6, A2 -> A3)
      def compare(ms: List[(Pos, Pos)], fen: String) =
        makeGame.playMoveList(ms) must beValid.like { g =>
          (f << FEN(fen)) must beSome.like { situation =>
            situation.board.visual must_== g.situation.board.visual
          }
        }
      "new game" in {
        compare(
          Nil,
          "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR r KQkq - 0 1"
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
          "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR r KQkq c6 0 2"
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
          "rnbqkb1r/pp1ppppp/7n/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R r KQkq - 2 3"
        )
      }
      "5 moves" in {
        compare(
          moves take 5,
          "rnbqkb1r/pp1ppppp/7n/2p5/4P3/P4N2/1PPP1PPP/RNBQKB1R b KQkq - 0 3"
        )
      }
      "invalid" in {
        f << FEN("hahaha") must beNone
      }
      // "weird" in {
      //   f << "raalll3qb1r/p1Q1nk1p/2n3p1/8/8/5N2/PPPP1PPP/RNB1K2R b KQ - 0 10" must beNone
      // }
      // "too long" in {
      //   f << "r3qb1r/p1Q1nk1p/2n3p1/8/8/5N2/PPPP1PPP/RNB1K2R/RNB1K2R b KQ - 0 10" must beNone
      // }
    }
  }
  "preserve variant" in {
    "chess960" in {
      f.<<@(Chess960, FEN("rnbqkb1r/pp1ppppp/7n/2p5/4P3/P4N2/1PPP1PPP/RNBQKB1R b KQkq - 0 3")) must beSome
        .like { s =>
          s.board.variant must_== Chess960
        }
    }
    "crazyhouse" in {
      f.<<<@(Crazyhouse, FEN("rnbqkb1r/pp1ppppp/7n/2p5/4P3/P4N2/1PPP1PPP/RNBQKB1R b KQkq - 0 3")) must beSome
        .like { s =>
          s.situation.board.variant must_== Crazyhouse
        }
    }
  }
  "export to situation plus" should {
    "with turns" in {
      "starting" in {
        f <<< FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR r KQkq - 0 1") must beSome.like { s =>
          s.turns must_== 0
        }
      }
      "red to play" in {
        f <<< FEN("r2q1rk1/ppp2pp1/1bnpbn1p/4p3/4P3/1BNPBN1P/PPPQ1PP1/R3K2R r KQ - 7 10") must beSome.like {
          s => s.turns must_== 18
        }
      }
      "black to play" in {
        f <<< FEN("r1q2rk1/ppp2ppp/3p1n2/8/2PNp3/P1PnP3/2QP1PPP/R1B2K1R b - - 3 12") must beSome.like { s =>
          s.turns must_== 23
        }
      }
      "last move (for en passant)" in {
        f <<< FEN("2b2rk1/3p2pp/2pNp3/4PpN1/qp1P3P/4P1K1/6P1/1Q6 r - f6 0 36") must beSome.like { s =>
          s.situation.board.history.lastMove must_== Option(Uci.Move(Pos.F7, Pos.F5))
        }
      }
      "last move (for en passant in Pretrov's defense)" in {
        f <<< FEN("rnbqkb1r/ppp2ppp/8/3pP3/3Qn3/5N2/PPP2PPP/RNB1KB1R r KQkq d6 0 6") must beSome.like { s =>
          s.situation.board.history.lastMove must_== Option(Uci.Move(Pos.D7, Pos.D5))
        }
      }
      "last move (for en passant with black to move)" in {
        f <<< FEN("4k3/8/8/8/4pP2/8/2K5/8 b - f3 0 1") must beSome.like { s =>
          s.situation.board.history.lastMove must_== Option(Uci.Move(Pos.F2, Pos.F4))
        }
      }
    }
    "with history" in {
      "starting" in {
        f <<< FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR r KQkq - 0 1") must beSome.like {
          case SituationPlus(
                Situation(Board(_, History(_, _, Castles(true, true, true, true), _, _, _), _, _), _),
                _
              ) =>
            ok
        }
      }
      "red to play" in {
        f <<< FEN("r2q1rk1/ppp2pp1/1bnpbn1p/4p3/4P3/1BNPBN1P/PPPQ1PP1/R3K2R r KQ - 7 10") must beSome.like {
          case SituationPlus(
                Situation(Board(_, History(_, _, Castles(true, true, false, false), _, _, _), _, _), _),
                _
              ) =>
            ok
        }
      }
      "black to play" in {
        f <<< FEN("r1q2rk1/ppp2ppp/3p1n2/8/2PNp3/P1PnP3/2QP1PPP/R1B2K1R b - - 3 12") must beSome.like {
          case SituationPlus(
                Situation(Board(_, History(_, _, Castles(false, false, false, false), _, _, _), _, _), _),
                _
              ) =>
            ok
        }
      }
    }
  }
  "fix impossible castle flags" should {
    def fixCastles(fen: FEN) = (f << fen).map(f >> _)
    "messed up" in {
      val fen = FEN("yayyyyyyyyyyy")
      fixCastles(fen) must beNone
    }
    "initial" in {
      val fen = FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR r KQkq - 0 1")
      fixCastles(fen) must beSome(fen)
    }
    "red king out" in {
      val fen = FEN("rnbqkbnr/pppppppp/8/8/4K3/8/PPPPPPPP/RNBQ1BNR r KQkq - 0 1")
      val fix = FEN("rnbqkbnr/pppppppp/8/8/4K3/8/PPPPPPPP/RNBQ1BNR r kq - 0 1")
      fixCastles(fen) must beSome(fix)
    }
    "black king out" in {
      val fen = FEN("rnbq1bnr/pppppppp/3k4/8/8/8/PPPPPPPP/RNBQKBNR r KQkq - 0 1")
      val fix = FEN("rnbq1bnr/pppppppp/3k4/8/8/8/PPPPPPPP/RNBQKBNR r KQ - 0 1")
      fixCastles(fen) must beSome(fix)
    }
    "red king rook out" in {
      val fen = FEN("rnbqkbnr/pppppppp/8/8/8/7R/PPPPPPPP/RNBQKBN1 r KQkq - 0 1")
      val fix = FEN("rnbqkbnr/pppppppp/8/8/8/7R/PPPPPPPP/RNBQKBN1 r Qkq - 0 1")
      fixCastles(fen) must beSome(fix)
    }
    "red queen rook out" in {
      val fen = FEN("rnbqkbnr/pppppppp/8/8/8/R7/PPPPPPPP/1NBQKBNR r KQkq - 0 1")
      val fix = FEN("rnbqkbnr/pppppppp/8/8/8/R7/PPPPPPPP/1NBQKBNR r Kkq - 0 1")
      fixCastles(fen) must beSome(fix)
    }
    "red king rook out" in {
      val fen = FEN("rnbqkbnr/pppppppp/8/8/8/7R/PPPPPPPP/RNBQKBN1 r KQkq - 0 1")
      val fix = FEN("rnbqkbnr/pppppppp/8/8/8/7R/PPPPPPPP/RNBQKBN1 r Qkq - 0 1")
      fixCastles(fen) must beSome(fix)
    }
    "red both rooks out" in {
      val fen = FEN("rnbqkbnr/pppppppp/8/8/8/R6R/PPPPPPPP/1NBQKBN1 r KQkq - 0 1")
      val fix = FEN("rnbqkbnr/pppppppp/8/8/8/R6R/PPPPPPPP/1NBQKBN1 r kq - 0 1")
      fixCastles(fen) must beSome(fix)
    }
    "red and black both rooks out" in {
      val fen = FEN("1nbqkbn1/pppppppp/8/8/8/R6R/PPPPPPPP/1NBQKBN1 r KQkq - 0 1")
      val fix = FEN("1nbqkbn1/pppppppp/8/8/8/R6R/PPPPPPPP/1NBQKBN1 r - - 0 1")
      fixCastles(fen) must beSome(fix)
    }
    "castling fixer regression" should {
      f <<< FEN("rk6/p1r3p1/P3B1K1/1p2B3/8/8/8/8 r - - 0 1") must beSome.like { s =>
        s.situation.board.history.castles.isEmpty
      }
      f <<< FEN("rk6/p1r3p1/P3B1K1/1p2B3/8/8/8/8 r - - 0 1") map f.>> must beSome.like { s =>
        s must_== FEN("rk6/p1r3p1/P3B1K1/1p2B3/8/8/8/8 r - - 0 1")
      }
      f << FEN("rk6/p1r3p1/P3B1K1/1p2B3/8/8/8/8 r - - 0 1") must beSome.like { s =>
        s.board.history.castles must_== Castles.none
      }
    }
    "castling not allowed in variant" in {
      val fen = FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR r KQkq - 0 1")
      val fix = FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR r - - 0 1")
      f.<<@(Antichess, fen).map(f >> _) must beSome(fix)
    }
  }
  "ignore impossible en passant squares" should {
    "with queen instead of pawn" in {
      f <<< FEN("8/4k3/8/6K1/1pp5/2q5/1P6/8 r - c3 0 1") must beSome.like { s =>
        s.situation.board.history.lastMove isEmpty
      }
    }
    "with no pawn" in {
      f <<< FEN("8/8/8/5k2/5p2/8/5K2/8 b - g3 0 1") must beSome.like { s =>
        s.situation.board.history.lastMove isEmpty
      }
    }
    "with non empty en passant squares" should {
      f <<< FEN("8/8/8/5k2/5pP1/8/6K1/8 b - g3 0 1") must beSome.like { s =>
        s.situation.board.history.lastMove isEmpty
      }
    }
    "with wrong side to move" should {
      f <<< FEN("8/8/8/5k2/5pP1/8/1K6/8 r - g3 0 1") must beSome.like { s =>
        s.situation.board.history.lastMove isEmpty
      }
    }
  }
  "crazyhouse" should {
    import variant.Crazyhouse._
    "read" in {
      "nope" in {
        f.<<<@(Crazyhouse, FEN("2b2rk1/3p2pp/2pNp3/4PpN1/qp1P3P/4P1K1/6P1/1Q6 r - f6 0 36")) must beSome
          .like { s =>
            s.situation.board.crazyData must_== Some(Data(Pockets(Pocket(Nil), Pocket(Nil)), Set.empty))
          }
      }
      "pockets are not confused as pieces" in {
        f.<<<@(Crazyhouse, FEN("2b2rk1/3p2pp/2pNp3/4PpN1/qp1P3P/4P1K1/6P1/1Q6/pPP r - f6 0 36")) must beSome
          .like { s =>
            f exportBoard s.situation.board must_== "2b2rk1/3p2pp/2pNp3/4PpN1/qp1P3P/4P1K1/6P1/1Q6"
          }
      }
      "pockets" in {
        f.<<<@(Crazyhouse, FEN("2b2rk1/3p2pp/2pNp3/4PpN1/qp1P3P/4P1K1/6P1/1Q6/pPP r - f6 0 36")) must beSome
          .like { s =>
            s.situation.board.crazyData must beSome.like {
              case Data(Pockets(Pocket(Pawn :: Pawn :: Nil), Pocket(Pawn :: Nil)), promoted) =>
                promoted must beEmpty
            }
          }
      }
      "winboard pockets" in {
        f.<<<@(
          Crazyhouse,
          FEN("r1bk3r/ppp2ppp/4p3/1B1pP3/1b1N4/2N2qPp/PPP2NbP/4R1KR[PNq] b - - 39 20")
        ) must beSome.like { s =>
          s.situation.board.crazyData must beSome.like {
            case Data(Pockets(Pocket(Pawn :: Knight :: Nil), Pocket(Queen :: Nil)), promoted) =>
              promoted must beEmpty
          }
        }
      }
      "promoted none" in {
        f.<<<@(Crazyhouse, FEN("2b2rk1/3p2pp/2pNp3/4PpN1/qp1P3P/4P1K1/6P1/1Q6/pPP r - f6 0 36")) must beSome
          .like { s =>
            s.situation.board.crazyData must beSome.like { case Data(_, promoted) =>
              promoted must beEmpty
            }
          }
      }
      "promoted some" in {
        f.<<<@(
          Crazyhouse,
          FEN("Q~R~b~2rk1/3p2pp/2pNp3/4PpN1/qp1P3P/4P1K1/6P1/1Q4q~R~/pPP r - f6 0 36")
        ) must beSome.like { s =>
          s.situation.board.crazyData must beSome.like { case Data(_, promoted) =>
            promoted must_== Set(A8, B8, C8, G1, H1)
          }
        }
      }
      "promoted on H8" in {
        f.<<<@(
          Crazyhouse,
          FEN("rnb1k2Q~/pp5p/2pp1p2/8/8/P1N2P2/P1PP1K1P/R1BQ1BNR/RPNBQPp b q - 21 11")
        ) must beSome.like { s =>
          s.situation.board.crazyData must beSome.like { case Data(_, promoted) =>
            promoted must_== Set(H8)
          }
        }
      }
      "promoted on H2" in {
        f.<<@(
          Crazyhouse,
          FEN("r2q1b1r/p2k1Ppp/2p2p2/4p3/P2nP2n/3P1PRP/1PPB1K1q~/RN1Q1B2/Npb r - - 40 21")
        ) must beSome
          .like { s =>
            s.board.crazyData must beSome.like { case Data(_, promoted) =>
              promoted must_== Set(H2)
            }
          }
      }
    }
  }
  "three-check" should {
    import variant.ThreeCheck
    "write" in {
      "no checks" in {
        val moves = List(E2 -> E4, C7 -> C5, G1 -> F3, G8 -> H6, A2 -> A3)
        Game(ThreeCheck).playMoveList(moves take 5) must beValid.like { g =>
          f >> g must_== FEN("rnbqkb1r/pp1ppppp/7n/2p5/4P3/P4N2/1PPP1PPP/RNBQKB1R b KQkq - 0 3 +0+0")
        }
      }
      "checks" in {
        val moves = List(E2 -> E4, E7 -> E5, F1 -> C4, G8 -> F6, B1 -> C3, F6 -> E4, C4 -> F7)
        Game(ThreeCheck).playMoveList(moves) must beValid.like { g =>
          f >> g must_== FEN("rnbqkb1r/pppp1Bpp/8/4p3/4n3/2N5/PPPP1PPP/R1BQK1NR b KQkq - 0 4 +1+0")
        }
      }
    }
    "read" in {
      "no checks" in {
        f <<< FEN("rnb1kbnr/pppp1ppp/8/4p3/4PP1q/8/PPPPK1PP/RNBQ1BNR b kq - 2 3") must beSome.like { s =>
          s.situation.board.history.checkCount.red must_== 0
          s.situation.board.history.checkCount.black must_== 0
        }
      }
      "explicitely no checks" in {
        f <<< FEN("rnb1kbnr/pppp1ppp/8/4p3/4PP1q/8/PPPPK1PP/RNBQ1BNR b kq - 2 3 +0+0") must beSome.like { s =>
          s.situation.board.history.checkCount.red must_== 0
          s.situation.board.history.checkCount.black must_== 0
        }
      }
      "checks" in {
        f <<< FEN("rnb1kbnr/pppp1ppp/8/4p3/4PP1q/8/PPPPK1PP/RNBQ1BNR b kq - 2 3 +1+2") must beSome.like { s =>
          s.situation.board.history.checkCount.red must_== 2
          s.situation.board.history.checkCount.black must_== 1
        }
      }
      "winboard checks" in {
        f <<< FEN("r1bqkbnr/pppp1Qpp/2n5/4p3/4P3/8/PPPP1PPP/RNB1KBNR b KQkq - 2+3 0 3") must beSome.like {
          s =>
            s.situation.board.history.checkCount.red must_== 0
            s.situation.board.history.checkCount.black must_== 1
        }
      }
    }
  }
  "x-fen" should {
    "wikipedia example" in {
      val canonical = FEN("rn2k1r1/ppp1pp1p/3p2p1/5bn1/P7/2N2B2/1PPPPP2/2BNK1RR r Gkq - 4 11")
      f <<< canonical must beSome.like { s =>
        s.situation.board.unmovedRooks must_== UnmovedRooks(Set(A8, G8, G1))
        f >> s must_== canonical
      }
    }
    "shredder fen of chess960 pos 284" in {
      f <<< FEN("rkbqrbnn/pppppppp/8/8/8/8/PPPPPPPP/RKBQRBNN r EAea - 0 1") must beSome.like { s =>
        s.situation.board.unmovedRooks must_== UnmovedRooks(Set(E1, E8, A1, A8))
        f >> s must_== FEN("rkbqrbnn/pppppppp/8/8/8/8/PPPPPPPP/RKBQRBNN r KQkq - 0 1")
      }
    }
    "wikipedia example" in {
      val canonical = FEN("rn2k1r1/ppp1pp1p/3p2p1/5bn1/P7/2N2B2/1PPPPP2/2BNK1RR r Gkq - 4 11")
      f <<< canonical must beSome.like { s =>
        s.situation.board.unmovedRooks must_== UnmovedRooks(Set(A8, G8, G1))
        f >> s must_== canonical
      }
    }
    "invalid castling rights" in {
      val board = Board.empty(Standard)
      f exportCastles board must_== "-"
    }
  }
}
