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
