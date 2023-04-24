package chess
package format
package pgn

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import Arbitraries.given

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
      node.mainline.forall(x => node.exists(_ == x))

  test("mainline.size + variations.size == node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size + variationsCount(node) == node.size

  test("find with mainline as path retuns last mainline node"):
    forAll: (node: Node[Int]) =>
      node.find(node.mainline).get == node.lastMainlineNode

  def variationsCount[A](node: Node[A]): Long =
    node.child.foldLeft(node.variation.fold(0L)(_.size))(_ + variationsCount(_))
