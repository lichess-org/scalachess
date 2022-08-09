package chess

import Pos._

class BoardTest extends ChessTest {

  val board = makeBoard

  "a board" should {

    "position pieces correctly" in {
      board.pieces must havePairs(
        A1 -> (Red - Rook),
        B1 -> (Red - Knight),
        C1 -> (Red - Bishop),
        D1 -> (Red - Queen),
        E1 -> (Red - King),
        F1 -> (Red - Bishop),
        G1 -> (Red - Knight),
        H1 -> (Red - Rook),
        A2 -> (Red - Pawn),
        B2 -> (Red - Pawn),
        C2 -> (Red - Pawn),
        D2 -> (Red - Pawn),
        E2 -> (Red - Pawn),
        F2 -> (Red - Pawn),
        G2 -> (Red - Pawn),
        H2 -> (Red - Pawn),
        A7 -> (Black - Pawn),
        B7 -> (Black - Pawn),
        C7 -> (Black - Pawn),
        D7 -> (Black - Pawn),
        E7 -> (Black - Pawn),
        F7 -> (Black - Pawn),
        G7 -> (Black - Pawn),
        H7 -> (Black - Pawn),
        A8 -> (Black - Rook),
        B8 -> (Black - Knight),
        C8 -> (Black - Bishop),
        D8 -> (Black - Queen),
        E8 -> (Black - King),
        F8 -> (Black - Bishop),
        G8 -> (Black - Knight),
        H8 -> (Black - Rook)
      )
    }

    "have pieces by default" in {
      board.pieces must not beEmpty
    }

    "have castling rights by default" in {
      board.history.castles == Castles.all
    }

    "allow a piece to be placed" in {
      board.place(Red - Rook, E3) must beSome.like { case b =>
        b(E3) mustEqual Option(Red - Rook)
      }
    }

    "allow a piece to be taken" in {
      board take A1 must beSome.like { case b =>
        b(A1) must beNone
      }
    }

    "allow a piece to move" in {
      board.move(E2, E4) must beSome.like { case b =>
        b(E4) mustEqual Option(Red - Pawn)
      }
    }

    "not allow an empty position to move" in {
      board.move(E5, E6) must beNone
    }

    "not allow a piece to move to an occupied position" in {
      board.move(A1, A2) must beNone
    }

    "allow a pawn to be promoted to a queen" in {
      makeEmptyBoard.place(Black.pawn, A8) flatMap (_ promote A8) must beSome.like { case b =>
        b(A8) must beSome(Black.queen)
      }
    }

    "allow chaining actions" in {
      makeEmptyBoard.seq(
        _.place(Red - Pawn, A2),
        _.place(Red - Pawn, A3),
        _.move(A2, A4)
      ) must beSome.like { case b =>
        b(A4) mustEqual Option(Red - Pawn)
      }
    }

    "fail on bad actions chain" in {
      makeEmptyBoard.seq(
        _.place(Red - Pawn, A2),
        _.place(Red - Pawn, A3),
        _.move(B2, B4)
      ) must beNone
    }

    "provide occupation map" in {
      makeBoard(
        A2 -> (Red - Pawn),
        A3 -> (Red - Pawn),
        D1 -> (Red - King),
        E8 -> (Black - King),
        H4 -> (Black - Queen)
      ).occupation must_== Color.Map(
        red = Set(A2, A3, D1),
        black = Set(E8, H4)
      )
    }

    "navigate in pos based on pieces" in {
      "right to end" in {
        val board: Board = """
R   K  R"""
        E1 >| (p => board.pieces contains p) must_== List(F1, G1, H1)
      }
      "right to next" in {
        val board: Board = """
R   KB R"""
        E1 >| (p => board.pieces contains p) must_== List(F1)
      }
      "left to end" in {
        val board: Board = """
R   K  R"""
        E1 |< (p => board.pieces contains p) must_== List(D1, C1, B1, A1)
      }
      "right to next" in {
        val board: Board = """
R  BK  R"""
        E1 |< (p => board.pieces contains p) must_== List(D1)
      }
    }
  }
}
