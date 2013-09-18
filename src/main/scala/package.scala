import ornicar.scalalib

package object chess

    extends scalalib.Validation
    with scalalib.Common
    with scalalib.OrnicarNonEmptyList

    with scalaz.syntax.std.ToBooleanOps

    with scalaz.std.OptionFunctions
    with scalaz.syntax.std.ToOptionOps
    with scalaz.syntax.std.ToOptionIdOps

    with scalaz.std.ListInstances
    with scalaz.syntax.std.ToListOps

    with scalaz.syntax.ToValidationOps
    with scalaz.syntax.ToFunctorOps
    with scalaz.syntax.ToIdOps {

  val White = Color.White
  val Black = Color.Black

  type Direction = Pos ⇒ Option[Pos]
  type Directions = List[Direction]

  object implicitFailures {
    implicit def stringToFailures(str: String): Failures = str.wrapNel
  }

  def parseIntOption(str: String): Option[Int] = try {
    Some(java.lang.Integer.parseInt(str))
  }
  catch {
    case e: NumberFormatException ⇒ None
  }
}
