package chess

import munit.DisciplineSuite
import org.scalacheck.*
import Arbitraries.given
import cats.laws.discipline.{ ApplicativeTests, FunctorTests, TraverseTests }

class ByColorLawsTest extends DisciplineSuite:
  checkAll("ByColor.FunctorLaws", FunctorTests[ByColor].functor[Int, Int, String])
  checkAll("ByColor.TraverseLaws", TraverseTests[ByColor].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("ByColor.ApplicativeLaws", ApplicativeTests[ByColor].applicative[Int, Int, String])
