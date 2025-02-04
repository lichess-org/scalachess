package chess

import cats.laws.discipline.{ ApplicativeTests, FunctorTests, TraverseTests }
import munit.DisciplineSuite

import CoreArbitraries.given

class ByColorLawsTest extends DisciplineSuite:
  checkAll("ByColor.FunctorLaws", FunctorTests[ByColor].functor[Int, Int, String])
  checkAll("ByColor.TraverseLaws", TraverseTests[ByColor].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("ByColor.ApplicativeLaws", ApplicativeTests[ByColor].applicative[Int, Int, String])
