package chess
package format

import munit.ScalaCheckSuite
import org.scalacheck.Prop
import UciArbitraries.given

class UciChairPairTest extends ScalaCheckSuite:

  test("convert move to pair"):
    assertEquals(UciCharPair(Uci.Move(Square.E2, Square.E4)).toString, "/?")

  test("convert drop to pair"):
    assertEquals(UciCharPair(Uci.Drop(Pawn, Square.C7)), UciCharPair('U', '\u008f'))
    assertEquals(UciCharPair(Uci.Drop(Knight, Square.C7)), UciCharPair('U', '\u008e'))
    assertEquals(UciCharPair(Uci.Drop(Bishop, Square.C7)), UciCharPair('U', '\u008d'))
    assertEquals(UciCharPair(Uci.Drop(Rook, Square.C7)), UciCharPair('U', '\u008c'))
    assertEquals(UciCharPair(Uci.Drop(Queen, Square.C7)), UciCharPair('U', '\u008b'))

  test("apply.toUci == identity"):
    Prop.forAll: (uci: Uci) =>
      assertEquals(UciCharPair(uci).toUci, uci)

  test("List[Uci] => UciPath => String => List[Uci]"):
    Prop.forAll: (xs: List[Uci]) =>
      val str = UciPath.fromIds(xs.map(UciCharPair(_))).debug
      assertEquals(str.split(" ").toList.map(Uci.apply).flatten, xs)

object UciArbitraries:

  import org.scalacheck.{ Arbitrary, Gen }
  import Arbitraries.given

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

  given Arbitrary[Uci] = Arbitrary(Gen.oneOf(normalUciMoveGen, promotionUciMoveGen, dropUciMoveGen))
