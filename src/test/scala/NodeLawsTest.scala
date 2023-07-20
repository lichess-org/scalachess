package chess

import cats.laws.discipline.FunctorTests
import munit.DisciplineSuite
import org.scalacheck.*
import Arbitraries.given
import cats.laws.discipline.TraverseTests

// class NodeLawTests extends DisciplineSuite:
//   checkAll("Node.FunctorLaws", FunctorTests[Node].functor[Int, Int, String])
//   checkAll("Node.TraverseLaws", TraverseTests[Node].traverse[Int, Int, Int, Int, Option, Option])
//   checkAll("Varitation.FunctorLaws", FunctorTests[Variation].functor[Int, Int, String])
//   checkAll("Varitation.TraverseLaws", TraverseTests[Variation].traverse[Int, Int, Int, Int, Option, Option])
