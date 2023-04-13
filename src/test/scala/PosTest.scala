package chess

import Square.*

class PosTest extends ChessTest:

  "A position" should:

    "be used to derive a relative list of positions" in:
      "D4 >| false" in { D4 >| (_ => false) must contain(E4, F4, G4, H4) }
      "D4 |< false" in { D4 |< (_ => false) must contain(C4, B4, A4) }
      "D4 >| (==G4)" in { D4 >| (G4 ==) must contain(E4, F4, G4) }
      "D4 |< (==C4)" in { D4 |< (C4 ==) must contain(C4) }

    "be a string" in:
      "D5" in { D5.key must_== "d5" }

    "chars" in:
      A1.asChar must_== 'a'
      B4.asChar must_== 'z'
      C4.asChar must_== 'A'
      D7.asChar must_== 'Z'
      E7.asChar must_== '0'
      F7.asChar must_== '1'
      F8.asChar must_== '9'
      G8.asChar must_== '!'
      H8.asChar must_== '?'
