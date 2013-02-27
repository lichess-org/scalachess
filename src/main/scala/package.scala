import ornicar.scalalib

package object chess
    extends scalalib.Validation
    with scalalib.Common
    with scalalib.NonEmptyLists 
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

  def parseIntOption(str: String): Option[Int] = try {
    Some(java.lang.Integer.parseInt(str))
  }
  catch {
    case e: NumberFormatException ⇒ None
  }
}
