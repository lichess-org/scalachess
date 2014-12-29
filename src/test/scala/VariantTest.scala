package chess

import Pos._
import Variant._

class VariantTest extends ChessTest {

  val board = makeBoard

  "standard" should {

    "position pieces correctly" in {
      Standard.pieces must havePairs(A1 -> (White - Rook), B1 -> (White - Knight), C1 -> (White - Bishop), D1 -> (White - Queen), E1 -> (White - King), F1 -> (White - Bishop), G1 -> (White - Knight), H1 -> (White - Rook), A2 -> (White - Pawn), B2 -> (White - Pawn), C2 -> (White - Pawn), D2 -> (White - Pawn), E2 -> (White - Pawn), F2 -> (White - Pawn), G2 -> (White - Pawn), H2 -> (White - Pawn), A7 -> (Black - Pawn), B7 -> (Black - Pawn), C7 -> (Black - Pawn), D7 -> (Black - Pawn), E7 -> (Black - Pawn), F7 -> (Black - Pawn), G7 -> (Black - Pawn), H7 -> (Black - Pawn), A8 -> (Black - Rook), B8 -> (Black - Knight), C8 -> (Black - Bishop), D8 -> (Black - Queen), E8 -> (Black - King), F8 -> (Black - Bishop), G8 -> (Black - Knight), H8 -> (Black - Rook))
    }
  }

  "chess960" should {

    "position pieces correctly" in {
      Chess960.pieces must havePair(A2 -> (White - Pawn))
    }
  }

  "kingOfTheHill" should {
    "detect win" in {
      "not" in {
        Game("""
PPk
K
""".kingOfTheHill, White).situation.end must beFalse
      }
      "regular checkMate" in {
        val game = Game("""
PP
K  r
""".kingOfTheHill, White)

        game.situation.end must beTrue
        game.situation.winner must beSome.like {
          case color =>
            color == Black
        }
      }
      "centered black king" in {
        val sit = Game("""
   k

PP
   K
""".kingOfTheHill, White).situation
        sit.end must beTrue
        sit.winner must beSome.like{
          case color => color == Black
        }

      }
    }
  }

  "threeCheck" should {
    "detect win" in {
      "not" in {
        Game("""
PPk
K
""".threeCheck, White).situation.end must beFalse
      }
      "regular checkMate" in {
        val game = Game("""
PP
K  r
""".threeCheck, White)
        game.situation.end must beTrue
        game.situation.winner must beSome.like {
          case color =>
            color == Black
        }
      }
      "1 check" in {
        val game = Game(Board init Variant.ThreeCheck).playMoves(
          E2 -> E4, E7 -> E6, D2 -> D4, F8 -> B4).toOption.get
        game.situation.end must beFalse
      }
      "2 checks" in {
        val game = Game(Board init Variant.ThreeCheck).playMoves(
          E2 -> E4, E7 -> E6, D2 -> D4, F8 -> B4, C2 -> C3, B4 -> C3).toOption.get
        game.situation.end must beFalse
      }
      "3 checks" in {
        val game = Game(Board init Variant.ThreeCheck).playMoves(
          E2 -> E4, E7 -> E6, D2 -> D4, F8 -> B4, C2 -> C3,
          B4 -> C3, B1 -> C3, D8 -> H4, A2 -> A3, H4 -> F2).toOption.get
        game.situation.end must beTrue

        game.situation.winner must beSome.like{
          case color =>
            color == Black
        }
      }
    }
  }
}
