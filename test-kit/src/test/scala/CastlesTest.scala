package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop

import CoreArbitraries.given

class CastlesTest extends ScalaCheckSuite:

  test("standard apply round-trip with booleans"):
    Prop.forAll:
      (whiteKingSide: Boolean, whiteQueenSide: Boolean, blackKingSide: Boolean, blackQueenSide: Boolean) =>
        val cr = CastlingRights(whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide)
        cr.contains(Square.H1) == whiteKingSide &&
        cr.contains(Square.A1) == whiteQueenSide &&
        cr.contains(Square.H8) == blackKingSide &&
        cr.contains(Square.A8) == blackQueenSide

  test("without(color) removes only that color's back-rank bits"):
    Prop.forAll { (cr: CastlingRights, color: Color) =>
      val updated = cr.without(color)
      val cleanedRank = Bitboard.rank(color.backRank).value
      val keptRank = Bitboard.rank((!color).backRank).value
      (updated.value & cleanedRank) == 0L &&
      (updated.value & keptRank) == (cr.value & keptRank)
    }

  test("bitwise & with complement of square removes that bit"):
    Prop.forAll { (cr: CastlingRights, color: Color) =>
      val sq = if color.white then Square.A1 else Square.A8
      val updated = cr & ~sq.bl
      !updated.contains(sq) &&
      // All other bits unchanged
      (updated.value | sq.bl) == (cr.value | sq.bl)
    }

  test("isEmpty iff no bits set"):
    Prop.forAll { (cr: CastlingRights) =>
      cr.isEmpty == (cr.value == 0L)
    }
