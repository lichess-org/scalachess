package chess
package format
package pgn

import cats.laws.discipline.FunctorTests
import munit.DisciplineSuite
import org.scalacheck.*
import NodeGen.given
import cats.laws.discipline.TraverseTests

class NodeLawTests extends DisciplineSuite:
  checkAll("Node.FunctorLaws", FunctorTests[Node].functor[Int, Int, String])
  checkAll("Node.TraverseLaws", TraverseTests[Node].traverse[Int, Int, Int, Int, Option, Option])
