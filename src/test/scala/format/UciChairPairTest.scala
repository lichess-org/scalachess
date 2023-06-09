package chess
package format

import munit.ScalaCheckSuite
import org.scalacheck.{ Arbitrary, Gen, Prop }
import Arbitraries.given

class UciChairPairTest extends ScalaCheckSuite:

  def normalUciMoveGen =
    for
      orig <- Arbitrary.arbitrary[Square]
      dest <- Arbitrary.arbitrary[Square]
    yield Uci.Move(orig, dest)

  def promotionUciMoveGen =
    for
      file   <- Arbitrary.arbitrary[File]
      rank   <- Gen.oneOf(Rank.Second, Rank.Seventh)
      role   <- Gen.oneOf(Role.allPromotable)
      offset <- Gen.oneOf(-1, 1)
      destFile = file.offset(offset).getOrElse(file)
      orig     = Square(file, rank)
      dest     = Square(destFile, UciCharPair.implementation.lastRank(orig))
    yield Uci.Move(orig, dest, Some(role))

  def dropUciMoveGen =
    for
      dest <- Arbitrary.arbitrary[Square]
      role <- Gen.oneOf(Pawn, Knight, Bishop, Rook, Queen)
    yield Uci.Drop(role, dest)

  test("identity rule for Uci.Move"):
    Prop.forAll(normalUciMoveGen): (uci: Uci) =>
      assertEquals(UciCharPair(uci).toUci, uci)

  test("identity rule for promotion Uci.Move"):
    Prop.forAll(promotionUciMoveGen): (uci: Uci) =>
      assertEquals(UciCharPair(uci).toUci, uci)

  test("identity rule for Uci.Drop"):
    Prop.forAll(dropUciMoveGen): (uci: Uci) =>
      assertEquals(UciCharPair(uci).toUci, uci)
