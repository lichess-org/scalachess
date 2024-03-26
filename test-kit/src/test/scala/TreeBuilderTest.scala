package chess

import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.{ forAll, propBoolean }

class TreeBuilderTest extends ScalaCheckSuite:

  test("build an empty list return none"):
    assertEquals(Tree.build(Nil), None)
    assertEquals(Tree.build(Nil, identity), None)
    assertEquals(Tree.buildWithIndex(Nil, ((_, _) => ())), None)
    assertEquals(Tree.build(List.empty[Node[Int]]), None)
    assertEquals(Tree.build[Int, Int](List.empty[Int], Node(_)), None)
    assertEquals(Tree.build[Int, Int, Option](Nil, Node(_).some), none.some)
    assertEquals(Tree.buildAccumulate[Int, Int, Int](Nil, 0, ((y, x) => (y, Node(x)))), None)
    assertEquals(
      Tree.buildAccumulate[Int, Int, Int, Option](Nil, 0, ((y, x) => (y, Node(x)).some)),
      none.some
    )

  test("build.mainLineValues == identity"):
    forAll: (xs: List[Int]) =>
      Tree.build(xs).fold(Nil)(_.mainlineValues) == xs
      Tree.build(xs, identity).fold(Nil)(_.mainlineValues) == xs

  test("buildWithNode.mainLineValues == identity"):
    forAll: (xs: List[Int]) =>
      Tree.build(xs.map(Node(_))).fold(Nil)(_.mainlineValues) == xs
      Tree.build[Int, Int](xs, Node(_)).fold(Nil)(_.mainlineValues) == xs

  test("buildWithIndex.mainLineValues == identity"):
    forAll: (xs: List[Int]) =>
      Tree.buildWithIndex(xs, _ -> _).fold(Nil)(_.mainlineValues.map(_._1)) == xs
      Tree.buildWithIndex(xs, _ -> _).fold(Nil)(_.mainlineValues.map(_._2)) == (0 until xs.size).toList

  test("buildWithNodeF(identity effect).mainLineValues == identity"):
    forAll: (xs: List[Int]) =>
      Tree.build[Int, Int, Option](xs, Node(_).some).flatten.fold(Nil)(_.mainlineValues) == xs

  test("buildWithNodeM.mainLineValues ~= traverse"):
    forAll: (xs: List[Int], f: Int => Option[Int]) =>
      Tree.build[Int, Int, Option](xs, f(_).map(Node(_))).map(_.fold(Nil)(_.mainlineValues)) ==
        xs.traverse(f)

  test("buildAccumulate"):
    forAll: (xs: List[Int]) =>
      Tree
        .buildAccumulate[Int, Long, Long](xs, 0L, (x, y) => (x + y) -> Node(x + y))
        .fold(0L)(_.lastMainlineNode.value) == xs.map(_.toLong).sum

  test("buildAccumulateM"):
    forAll: (xs: List[Int]) =>
      Tree
        .buildAccumulate[Int, Long, Long, Option](xs, 0L, (x, y) => ((x + y) -> Node(x + y)).some)
        .flatten
        .map(_.lastMainlineNode.value)
        .getOrElse(0L) == xs.map(_.toLong).sum

  test("buildAccumulateM ~= traverse"):
    forAll: (xs: List[Int], f: Int => Option[Int]) =>
      Tree
        .buildAccumulate[Int, Int, Int, Option](xs, 0, (_, y) => f(y).map(0 -> Node(_)))
        .map(_.fold(Nil)(_.mainlineValues)) == xs.traverse(f)
