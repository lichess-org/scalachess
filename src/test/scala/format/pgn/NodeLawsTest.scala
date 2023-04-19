package chess
package format
package pgn

import cats.syntax.all.*
import cats.laws.discipline.FunctorTests
import munit.DisciplineSuite
import org.scalacheck.*
import Arbitraries.given
import cats.laws.discipline.TraverseTests
import cats.Eval
import cats.Foldable

class NodeLawTests extends DisciplineSuite:
  checkAll("Node.FunctorLaws", FunctorTests[Node].functor[Int, Int, String])
  checkAll("Node.TraverseLaws", TraverseTests[Node].traverse[Int, Int, Int, Int, Option, Option])
