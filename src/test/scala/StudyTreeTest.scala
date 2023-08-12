package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import TreeArbitraries.*
import chess.variant.Standard

class StudyTreeTest extends ScalaCheckSuite:

  // test("StudyTreeTest"):
  //   forAll: (tree: Node[Situation]) =>
  //     tree.size.pp > 0

  test("GameTree"):
    forAll(genTree(Situation(Standard))): tree =>
      tree.tree.forall(_.size > 0)
