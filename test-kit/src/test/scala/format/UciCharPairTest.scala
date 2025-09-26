package chess
package format

import munit.ScalaCheckSuite
import org.scalacheck.Prop

import CoreArbitraries.given

class UciCharPairTest extends ScalaCheckSuite:

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
