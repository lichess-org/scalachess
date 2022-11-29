package chess

import Pos.*

class PosTest extends ChessTest:

  "A position" should {

    "be used to derive a relative list of positions" in {
      "D4 >| false" in { D4 >| (_ => false) must contain(E4, F4, G4, H4) }
      "D4 |< false" in { D4 |< (_ => false) must contain(C4, B4, A4) }
      "D4 >| (==G4)" in { D4 >| (G4 ==) must contain(E4, F4, G4) }
      "D4 |< (==C4)" in { D4 |< (C4 ==) must contain(C4) }
    }

    "be a string" in {
      A1.key.pp
      H8.key.pp
      "D5" in { D5.key must_== "d5" }
    }

    "piotr" in {
      A1.toChar must_== 'a'
      B4.toChar must_== 'z'
      C4.toChar must_== 'A'
      D7.toChar must_== 'Z'
      E7.toChar must_== '0'
      F7.toChar must_== '1'
      F8.toChar must_== '9'
      G8.toChar must_== '!'
      H8.toChar must_== '?'
    }
  }
