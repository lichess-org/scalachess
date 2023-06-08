package chess
package format

import chess.Square

class UciPathTest extends munit.FunSuite:

  test("empty intersect"):
    assertEquals(UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5").intersect(UciPath(")8VN")), UciPath.root)

  test("full intersect"):
    val p = UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5")
    assertEquals(p.intersect(p), p)

  test("partial left"):
    assertEquals(UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5").intersect(UciPath("/?UE)8")), UciPath("/?UE)8"))

  test("partial right"):
    assertEquals(UciPath("/?UE)8").intersect(UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5")), UciPath("/?UE)8"))

  test("depth"):
    val p = UciPath(
      """.>VF-=F=/?WG)8`<%.<.&.G>8>aP$5^W'#_b08UE>-\M(=]O=OXO.N[^NWMW&^`^*&^&5&aX,<E<-<OG&7XO%-PV-5VE5=E;<6;L=DLV6EVEDEYI19"""
    )
    assertEquals(p.depth, 57)

  test("convert move to pair"):
    assertEquals(UciCharPair(Uci.Move(Square.E2, Square.E4)).toString, "/?")

  test("convert drop to pair"):
    assertEquals(UciCharPair(Uci.Drop(Pawn, Square.C7)), UciCharPair('U', '\u008f'))
    assertEquals(UciCharPair(Uci.Drop(Knight, Square.C7)), UciCharPair('U', '\u008e'))
    assertEquals(UciCharPair(Uci.Drop(Bishop, Square.C7)), UciCharPair('U', '\u008d'))
    assertEquals(UciCharPair(Uci.Drop(Rook, Square.C7)), UciCharPair('U', '\u008c'))
    assertEquals(UciCharPair(Uci.Drop(Queen, Square.C7)), UciCharPair('U', '\u008b'))
