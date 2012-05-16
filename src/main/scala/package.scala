import ornicar.scalalib

package object chess
    extends scalalib.OrnicarValidation
    with scalalib.OrnicarCommon
    with scalalib.OrnicarNonEmptyLists 
    with scalaz.NonEmptyLists
    with scalaz.Strings
    with scalaz.Lists
    with scalaz.Booleans {

  val White = Color.White
  val Black = Color.Black

  type Direction = Pos ⇒ Option[Pos]
  type Directions = List[Direction]

  object implicitFailures {
    implicit def stringToFailures(str: String): Failures = str wrapNel
  }
}
