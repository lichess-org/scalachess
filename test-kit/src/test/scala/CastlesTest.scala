package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop

import CoreArbitraries.given
import Castles.*

class CastlesTest extends ScalaCheckSuite:

  test("can(color, side) should be consistent with properties"):
    Prop.forAll { (c: Castles) =>
      (c.can(White, KingSide) == c.whiteKingSide) &&
      (c.can(White, QueenSide) == c.whiteQueenSide) &&
      (c.can(Black, KingSide) == c.blackKingSide) &&
      (c.can(Black, QueenSide) == c.blackQueenSide)
    }

  test("apply with booleans"):
    Prop.forAll:
      (whiteKingSide: Boolean, whiteQueenSide: Boolean, blackKingSide: Boolean, blackQueenSide: Boolean) =>
        val c = Castles(whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide)
        (c.can(White, KingSide) == whiteKingSide) &&
        (c.can(White, QueenSide) == whiteQueenSide) &&
        (c.can(Black, KingSide) == blackKingSide) &&
        (c.can(Black, QueenSide) == blackQueenSide)

  test("without color"):
    Prop.forAll { (c: Castles, color: Color) =>
      val updated = c.without(color)
      updated.can(color) == false &&
      updated.can(!color) == c.can(!color)
    }

  test("without color and side"):
    Prop.forAll { (c: Castles, color: Color, side: Side) =>
      val updated = c.without(color, side)
      updated.can(color, side) == false &&
      updated.can(color, !side) == c.can(color, !side) &&
      updated.can(!color) == c.can(!color)
    }

  test("add"):
    Prop.forAll { (c: Castles, color: Color, side: Side) =>
      val updated = c.add(color, side)
      updated.can(color) == true &&
      updated.can(!color) == c.can(!color)
    }

  test("update"):
    Prop.forAll { (c: Castles, color: Color, kingSide: Boolean, queenSide: Boolean) =>
      val updated = c.update(color, kingSide, queenSide)
      updated.can(color, KingSide) == kingSide &&
      updated.can(color, QueenSide) == queenSide &&
      updated.can(!color) == c.can(!color)
    }
