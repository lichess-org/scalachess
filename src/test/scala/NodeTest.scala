package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import Arbitraries.given
import org.scalacheck.Prop.propBoolean
import scala.util.Random

class NodeTest extends ScalaCheckSuite:

  given HasId[Int, Int] with
    def getId(a: Int): Int = a

  test("use mainline as path for findPath"):
    forAll: (node: Tree[Int]) =>
      node.findPath(node.mainlineValues) == node.mainline.some

  test("use subset of mainline as path for findPath"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.size >= 2 ==> {
        val size = Random.nextInt(node.mainlineValues.size - 1) + 1
        val path = node.mainlineValues.take(size)
        node.findPath(path).isDefined == true
      }

  test("use randomPath for findPath"):
    forAll: (node: Node[Int]) =>
      val path = node.randomPath
      path.nonEmpty ==> {
        val found = node.findPath(path).map(_.map(_.value))
        found.isEmpty || (found.isDefined && found.get == path)
      }

  // test("modifyAt with mainline == modifyLastMainlineNode"):
  //   forAll: (node: Node[Int], f: Int => Int) =>
  //     def m(n: Node[Int]) = n.copy(value = f(n.value))
  //     node.modifyAt(node.mainlineValues, m) == node.modifyLastMainlineNode(m).some

  test("modifyAt and find are consistent"):
    forAll: (node: Node[Int]) =>
      val path = node.randomPath
      def f[A]: TreeModifier[A] = node =>
        node match
          case n: Node[A]      => n
          case v: Variation[A] => v
      node.modifyAt(path, f).flatMap(_.find(path)) == node.find(path)

  // test("find and exist are consistent"):
  //   forAll: (node: Node[Int], n: Int) =>
  //     node.find(List(n)).isDefined == node.exists(_ == n)

  extension [A](node: Node[A])
    // def variationsCount: Long =
    //   node.child.foldLeft(node.variation.fold(0L)(_.size))((acc, v) => acc + v.variationsCount)

    // generate a path from the root to a random node
    def randomPath: List[A] =
      if Random.nextBoolean() then node.value :: node.child.fold(List.empty[A])(_.randomPath)
      else if node.variations.size <= 1 then Nil
      else if Random.nextBoolean() then
        val v = node.variations(Random.nextInt(node.variations.size - 1))
        v.value :: v.child.fold(List.empty[A])(_.randomPath)
      else Nil
