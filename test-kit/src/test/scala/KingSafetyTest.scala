package chess

import scala.language.implicitConversions
import Square.*

class KingSafetyTest extends ChessTest:

  import compare.dests

  test("not commit suicide"):
    assertEquals(
      """
    P n
PPPP   P
RNBQK  R""" `destsFrom` E1,
      Set(F2)
    )
  test("not commit suicide even if immobilized"):
    assertEquals(
      """
    b n
PPPP   P
RNBQK  R""" `destsFrom` E1,
      Set()
    )
  test("escape from danger"):
    assertEquals(
      """
    r

PPPP   P
RNBQK  R""" `destsFrom` E1,
      Set(F1, F2)
    )
  test("move to defend"):
    assertEquals(
      """
    r

PPPP   P
RNBQK  R""" `destsFrom` D1,
      Set(E2)
    )
    assertEquals(
      """
    r

PPPP   P
RNBQK NR""" `destsFrom` G1,
      Set(E2)
    )
    assertEquals(
      """
K    r
PPPP   P
RNBQ  NR""" `destsFrom` D2,
      Set(D3)
    )
    assertEquals(
      """
K    r

PPPP   P
RNBQ  NR""" `destsFrom` D2,
      Set(D4)
    )
    assertEquals(
      """
K    r

PPPP   P
RNBQ  NR""" `destsFrom` H2,
      Set()
    )
  assertEquals(
    """
    r

PPPPK Q
RNB    R""" `destsFrom` G2,
    Set(E4)
  )
  assertEquals(
    """
    r

PPPPQ
RNB K  R""" `destsFrom` E2,
    Set(E3, E4)
  )
  assertEquals(
    """
    r
     P
PPPP
RNB K  R""" `destsFrom` F3,
    Set(E4)
  )
  test("stay to defend"):
    assertEquals(
      """
    r

PPPPB
RNB K  R""" `destsFrom` E2,
      Set()
    )
    assertEquals(
      """

K P  r
PPP
RNB    R""" `destsFrom` D3,
      Set()
    )
