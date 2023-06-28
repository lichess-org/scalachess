package chess

import Square.*

class SquareTest extends munit.FunSuite:

  test("keys"):
    assertEquals(D5.key, "d5")

  test("chars"):
    assertEquals(A1.asChar, 'a')
    assertEquals(B4.asChar, 'z')
    assertEquals(C4.asChar, 'A')
    assertEquals(D7.asChar, 'Z')
    assertEquals(E7.asChar, '0')
    assertEquals(F7.asChar, '1')
    assertEquals(F8.asChar, '9')
    assertEquals(G8.asChar, '!')
    assertEquals(H8.asChar, '?')
