package chess

import cats.syntax.all.*
import Square.*
import munit.ScalaCheckSuite
import CoreArbitraries.given
import org.scalacheck.Prop.forAll

class SquareTest extends ScalaCheckSuite:

  test("keys"):
    assertEquals(D5.key, "d5")

  test("chars for some squares"):
    assertEquals(A1.asChar, 'a')
    assertEquals(B4.asChar, 'z')
    assertEquals(C4.asChar, 'A')
    assertEquals(D7.asChar, 'Z')
    assertEquals(E7.asChar, '0')
    assertEquals(F7.asChar, '1')
    assertEquals(F8.asChar, '9')
    assertEquals(G8.asChar, '!')
    assertEquals(H8.asChar, '?')

  val allChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!?"

  test("Square.asChar"):
    assertEquals(Square.all.map(_.asChar).mkString, allChars)

  test("chars are unique"):
    assertEquals(allChars.toList.traverse(Square.fromChar(_)).get, Square.all)

  test("keys.fromKey == some.identity"):
    Square.all.foreach: square =>
      assertEquals(Square.fromKey(square.key), square.some)

  test("keys are unique"):
    Square.all.map(_.key).toSet.size == 64

  test("x onSame x == true"):
    Square.all.foreach: square =>
      assert(square onSameLine square)
      assert(square onSameRank square)
      assert(square onSameDiagonal square)

  test("x onSame y == y onSame x"):
    forAll: (x: Square, y: Square) =>
      assertEquals(x onSameLine y, y onSameLine x)
      assertEquals(x onSameRank y, y onSameRank x)
      assertEquals(x onSameDiagonal y, y onSameDiagonal x)
