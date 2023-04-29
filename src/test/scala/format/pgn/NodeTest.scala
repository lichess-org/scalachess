package chess
package format
package pgn

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import Arbitraries.given
import org.scalacheck.Prop.propBoolean
import scala.util.Random

class NodeTest extends ScalaCheckSuite:

  test("filterOptional isEmpty"):
    forAll: (node: Node[Int], p: Int => Boolean) =>
      val optional = Node.filterOptional(p)
      optional.isEmpty(node) == node.forall(!p(_))

  test("filterOptional nonEmpty"):
    forAll: (node: Node[Int], p: Int => Boolean) =>
      val optional = Node.filterOptional(p)
      optional.nonEmpty(node) == node.exists(p)

  test("replace and find are consistent"):
    forAll: (node: Node[Int], p: Int => Boolean, newNode: Node[Int]) =>
      val original = node.findNode(p)
      val result   = node.replaceNode(p)(newNode)
      (original, result) match
        case (Some(_), Some(_)) => true
        case (None, None)       => true
        case _                  => false

  test("delete and find are consistent"):
    forAll: (node: Node[Int], p: Int => Boolean) =>
      val original = node.findNode(p)
      val result   = node.deleteSubNode(p)
      original == node.some || {
        (original, result) match
          case (Some(x), Some(y)) => x.size + y.size == node.size
          case (None, None)       => true
          case _                  => false
      }

  test("filterTraversal"):
    forAll: (node: Node[Int]) =>
      val filter = Node.filterTraversal[Int](_ % 2 != 0)
      val result = filter.modify(_ * 2)(node)
      result.forall(_ % 2 == 0)

  test("filterOptional can be used instead of replace"):
    forAll: (node: Node[Int], p: Int => Boolean, newNode: Node[Int]) =>
      val filter       = Node.filterOptional(p)
      val withOptional = filter.replaceOption(newNode)(node)
      val direct       = node.replaceNode(p)(newNode)
      withOptional == direct

  test("filterOptional can be used instead of modify"):
    forAll: (node: Node[Int], p: Int => Boolean, f: Int => Int) =>
      val filter       = Node.filterOptional(p)
      val withOptional = filter.modifyOption(x => x.map(f))(node)
      val direct       = node.modifyNode(p)(x => x.map(f))
      withOptional == direct

  test("mainline size <= node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size <= node.size

  test("mainline is a sub set of node"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.forall(x => node.exists(_ == x))

  test("mainline.size + variations.size == node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size + variationsCount(node) == node.size

  test("find ids with mainline as path retuns last mainline node"):
    forAll: (node: Node[Int]) =>
      node.find(node.mainlineValues).get == node.lastMainlineNode

  test("use mainline as path for findPath"):
    forAll: (node: Node[Int]) =>
      node.findPath(node.mainlineValues) == node.mainline.some

  test("use subset of mainline as path for findPath"):
    forAll: (node: Node[Int]) =>
      if node.mainlineValues.size >= 2 then
        val size = Random.nextInt(node.mainlineValues.size - 1) + 1
        val path = node.mainlineValues.take(size)
        node.findPath(path).isDefined == true
      else true

  test("use randomPath for findPath"):
    forAll: (node: Node[Int]) =>
      val path = node.randomPath
      if path.isEmpty then true
      else
        val found = node.findPath(path).map(_.map(_.value))
        found.isEmpty || (found.isDefined && found.get == path)

  test("find"):
    forAll: (node: Node[Int], n: Int) =>
      node.find(n).isDefined == node.exists(_ == n)

  test("findNode and find are consistent"):
    forAll: (node: Node[Int], n: Int) =>
      node.findNode(_ == n) == node.find(n)

  test("modifyAt with mainline == modifyLastMainlineNode"):
    forAll: (node: Node[Int], f: Int => Int) =>
      def m(n: Node[Int]) = n.copy(value = f(n.value))
      node.modifyAt(node.mainlineValues, m) == node.modifyLastMainlineNode(m).some

  test("modifyAt and findAt are consistent"):
    forAll: (node: Node[Int], f: Int => Int) =>
      val path = node.randomPath
      node.modifyAt(path, identity).flatMap(_.find(path)) == node.find(path)

  test("deleteAt with root value return Some(None)"):
    forAll: (node: Node[Int]) =>
      node.deleteAt(List(node.value)) == Some(None)

  test("deleteAt and findAt are consistent"):
    forAll: (node: Node[Int]) =>
      val path = node.randomPath
      node.deleteAt(path).isDefined == node.find(path).isDefined

  extension [A](node: Node[A])
    def variationsCount: Long =
      node.child.foldLeft(node.variation.fold(0L)(_.size))((acc, v) => acc + v.variationsCount)

    def randomPath: List[A] =
      if Random.nextBoolean() then node.value :: node.child.fold(List.empty[A])(_.randomPath)
      else if Random.nextBoolean() then node.variation.fold(List.empty[A])(_.randomPath)
      else Nil
