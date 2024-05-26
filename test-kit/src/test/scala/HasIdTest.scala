package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.{ forAll, propBoolean }

class HasIdTest extends ScalaCheckSuite:

  given HasId[Int, Int] with
    extension (a: Int) def id: Int = a

  test("if there is no id, removeById does nothing"):
    forAll: (xs: List[Int], id: Int) =>
      xs.indexOf(id) == -1 ==> (xs.removeById(id) == xs)

  test("removeById.size <= size"):
    forAll: (xs: List[Int], id: Int) =>
      val removed = xs.removeById(id)
      val epsilon = if xs.find(_ == id).isDefined then 1 else 0
      xs.size == removed.size + epsilon

  test("removeById reserves order"):
    forAll: (xs: List[Int], id: Int) =>
      val sorted  = xs.sorted
      val removed = sorted.removeById(id)
      removed == removed.sorted

  test("removeById only remove first items"):
    val xs = List(1, 1)
    xs.removeById(1) == List(1)
