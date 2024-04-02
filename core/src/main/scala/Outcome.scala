package chess

case class Outcome(winner: Option[Color]):
  override def toString = winner match
    case Some(White) => "1-0"
    case Some(Black) => "0-1"
    case None        => "1/2-1/2"

object Outcome:
  def showResult(outcome: Option[Outcome]): String =
    outcome.fold("*")(_.toString)

  def fromResult(result: String): Option[Outcome] =
    normalizationMap.get(result)

  val white = Outcome(Some(White))
  val black = Outcome(Some(Black))
  val draw  = Outcome(None)

  lazy val knownResultStrings = "*" :: normalizationMap.keys.toList

  private val normalizationMap: Map[String, Outcome] =
    val hyphen     = "-"
    val enDash     = "–"
    val emDash     = "—"
    val dashes     = List(hyphen, enDash, emDash)
    val separators = dashes ::: List("_", ":")
    val draws      = List("½", "1/2")
    val wins       = List("1", "+")
    val losses     = "0" :: dashes

    val allDraws = for
      separator <- separators
      draw      <- draws
    yield s"$draw$separator$draw"
    val allWins = for
      separator <- separators
      win       <- wins
      loss      <- losses
    yield s"$win$separator$loss"
    val allLosses = for
      separator <- separators
      win       <- wins
      loss      <- losses
    yield s"$loss$separator$win"

    val pairs = allDraws.map(_ -> draw) :::
      allWins.map(_ -> white) :::
      allLosses.map(_ -> black)

    val lccResults = Map(
      "WHITEWIN" -> white,
      "BLACKWIN" -> black,
      "DRAW"     -> draw // ? not sure
    )

    pairs.toMap ++ lccResults
