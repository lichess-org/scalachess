package chess
package format

class UciPathTest extends ChessTest:

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

  test("debug"):
    val debug = UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5").debug
    assertEquals(debug, "e2e4 c7c5 g1f3 b8c6 f1b5 g7g6 b5c6 b7c6 e1h1 f8g7 f1e1 g8h6 c2c3")
