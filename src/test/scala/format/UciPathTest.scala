package chess.format

import org.specs2.mutable.*
import chess.Square

class UciPathTest extends Specification:

  "UciCharPair" should:
    "format" in:
      UciCharPair(Uci.Move(Square.E2, Square.E4)).toString === "/?"

  "intersect" should:
    "nope" in:
      UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5").intersect(UciPath(")8VN")) === UciPath.root
    "full" in:
      val p = UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5")
      p.intersect(p) === p
    "partial left" in:
      UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5").intersect(UciPath("/?UE)8")) === UciPath("/?UE)8")
    "partial right" in:
      UciPath("/?UE)8").intersect(UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5")) === UciPath("/?UE)8")
  "utils" in:
    val p = UciPath(
      """.>VF-=F=/?WG)8`<%.<.&.G>8>aP$5^W'#_b08UE>-\M(=]O=OXO.N[^NWMW&^`^*&^&5&aX,<E<-<OG&7XO%-PV-5VE5=E;<6;L=DLV6EVEDEYI19"""
    )
    p.depth === 57
