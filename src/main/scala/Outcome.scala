package chess

case class Outcome(winner: Option[Color]) {
  override def toString = winner match {
    case Some(White) => "1-0"
    case Some(Black) => "0-1"
    case None        => "1/2-1/2"
  }
}

object Outcome {
  def showResult(outcome: Option[Outcome]): String =
    outcome.fold("*")(_.toString)

  def fromResult(result: String): Option[Outcome] =
    result match {
      case "1-0"     => Some(Outcome(Some(White)))
      case "0-1"     => Some(Outcome(Some(Black)))
      case "1/2-1/2" => Some(Outcome(None))
      case _         => None
    }
}
