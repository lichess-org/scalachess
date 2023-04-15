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
          case (Some(x), Some(y)) => x.totalNodes + y.totalNodes == node.totalNodes
          case (None, None)       => true
          case _                  => false
      }
