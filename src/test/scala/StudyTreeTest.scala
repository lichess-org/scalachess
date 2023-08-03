package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import TreeArbitraries.given

class StudyTreeTest extends ScalaCheckSuite:

  test("StudyTreeTest"):
    forAll: (tree: Node[Situation]) =>
      tree.mainline.size.pp
      tree.size.pp > 0

  test("GameTree"):
    forAll: (tree: GameTree) =>
      tree.tree.forall(_.size.pp > 0)
