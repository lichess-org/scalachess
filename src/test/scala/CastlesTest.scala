package chess

import Pos.*
import Castles.*

class CastlesTest extends ChessTest:

  // todo add a test case for Atomic when a rook is exploded
  // todo more sophisicated tests


  "castle object" should {

    "init" in {
      val castles: Castles = Castles.all
      castles.whiteKingSide mustEqual true
      castles.whiteQueenSide mustEqual true
      castles.blackKingSide mustEqual true
      castles.blackQueenSide mustEqual true
    }

    "without White" in {
      val castles: Castles = Castles.all.without(White)
      castles.whiteKingSide mustEqual false
      castles.whiteQueenSide mustEqual false
      castles.blackKingSide mustEqual true
      castles.blackQueenSide mustEqual true
    }

    "without Black" in {
      val castles: Castles = Castles.all.without(Black)
      castles.whiteKingSide mustEqual true
      castles.whiteQueenSide mustEqual true
      castles.blackKingSide mustEqual false
      castles.blackQueenSide mustEqual false
    }

    "without Black" in {
      val castles: Castles = Castles.all.without(Black)
      castles.whiteKingSide mustEqual true
      castles.whiteQueenSide mustEqual true
      castles.blackKingSide mustEqual false
      castles.blackQueenSide mustEqual false
    }

    "without White Kingside" in {
      val castles: Castles = Castles.all.without(White, KingSide)
      castles.whiteKingSide mustEqual false
      castles.whiteQueenSide mustEqual true
      castles.blackKingSide mustEqual true
      castles.blackQueenSide mustEqual true
    }

    "without White QueenSide" in {
      val castles: Castles = Castles.all.without(White, QueenSide)
      castles.whiteKingSide mustEqual true
      castles.whiteQueenSide mustEqual false
      castles.blackKingSide mustEqual true
      castles.blackQueenSide mustEqual true
    }

    "update" in {
      val castles: Castles = Castles.all.update(White, false, true)
      castles.whiteKingSide mustEqual false
      castles.whiteQueenSide mustEqual true
      castles.blackKingSide mustEqual true
      castles.blackQueenSide mustEqual true
    }
  }
