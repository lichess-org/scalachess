package chess.format

import org.specs2.mutable.*

class UciPathTest extends Specification {

  "intersect" >> {
    "nope" >> {
      UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5").intersect(UciPath(")8VN")) === UciPath.root
    }
    "full" >> {
      val p = UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5")
      p.intersect(p) === p
    }
    "partial left" >> {
      UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5").intersect(UciPath("/?UE)8")) === UciPath("/?UE)8")
    }
    "partial right" >> {
      UciPath("/?UE)8").intersect(UciPath("/?UE)8\\M(DYQDMTM'*`Y('aR-5")) === UciPath("/?UE)8")
    }
  }
  "utils" >> {
    val p = UciPath(
      """.>VF-=F=/?WG)8`<%.<.&.G>8>aP$5^W'#_b08UE>-\M(=]O=OXO.N[^NWMW&^`^*&^&5&aX,<E<-<OG&7XO%-PV-5VE5=E;<6;L=DLV6EVEDEYI19"""
    )
    p.depth === 57
  }
}
