package chess
package opening

import org.specs2.mutable.Specification

class FullOpeningTest extends Specification {

  def searchStr(str: String) = FullOpeningDB search str.split(' ').toList

  "search" should {
    // "find nothing on invalid PGN" in {
    //   searchStr("e4 c5 Nf3 cxd4 d4 cxd4 Nxd4 e5 Nb5 d6 c4 a6 N5c3 Nf6 Be2 Be7 O-O-10. Be6 11Na3 Rc8 Rc1 Nb4 Qd2 Qd7 f3 Qe8 Nd5 Nbxd5 cxd5 Bd7 Rxc8 Bxc8 Rc1 Bd7 Nc4 b5 Nb6 Bd8") must beNone
    // }
    "find Kalashnikov" in {
      searchStr("e4 c5 Nf3 Nc6 d4 cxd4 Nxd4 e5 Nb5 d6 c4 a6 N5c3 Nf6 Be2 Be7 O-O-10. Be6 11Na3 Rc8 Rc1 Nb4 Qd2 Qd7 f3 Qe8 Nd5 Nbxd5 cxd5 Bd7 Rxc8 Bxc8 Rc1 Bd7 Nc4 b5 Nb6 Bd8") must beSome.like {
        case o => o.name == "Kalashnikov"
      }
    }
  }
}
