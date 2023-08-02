package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import SituationArbitraries.given

class StudyTreeTest extends ScalaCheckSuite:

  test("StudyTreeTest"):
    forAll: (tree: Node[Situation]) =>
      tree.mainline.size.pp
      tree.size.pp > 0
