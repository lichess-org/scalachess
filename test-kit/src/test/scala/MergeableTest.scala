package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import Foo.given

class MergeableTest extends ScalaCheckSuite:

  test("add size"):
    forAll: (xs: List[Foo], foo: Foo) =>
      val added = xs.add(foo)
      val diff  = if xs.exists(_.sameId(foo)) then 0 else 1
      xs.size == added.size - diff

  test("add size"):
    forAll: (xs: List[Foo], other: List[Foo]) =>
      val added = xs.add(other)
      added.size >= xs.size && added.size <= xs.size + other.size

  test("associativity"):
    forAll: (xs: List[Foo], ys: List[Foo], zs: List[Foo]) =>
      val left  = xs.add(ys).add(zs)
      val right = xs.add(ys.add(zs))
      left.size == right.size

  test("merge.merge == merge"):
    forAll: (xs: List[Foo]) =>
      xs.merge == xs.merge.merge

  test("after merge, ids are unique"):
    forAll: (xs: List[Foo]) =>
      val merged = xs.merge
      merged.size == merged.map(_.id).toSet.size
