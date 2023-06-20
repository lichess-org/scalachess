package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class HasIdTest extends ScalaCheckSuite:

  given HasId[Int, Int] with
    extension (a: Int)
      def id: Int = a

  test("removeById.size <= size"):
    forAll: (xs: List[Int], id: Int) =>
      val removed = xs.removeById(id)
      xs.size == removed.size || xs.size == removed.size + 1

  test("removeById.size <= size"):
    forAll: (xs: List[Int], id: Int) =>
      val removed = xs.removeById(id)
      val epsilon = if xs.find(_ == id).isDefined then 1 else 0
      xs.size == removed.size + epsilon
