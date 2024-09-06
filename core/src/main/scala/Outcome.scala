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
    normalizationMap.get(result).flatMap(fromPoints)

  val white = Outcome(Some(White))
  val black = Outcome(Some(Black))
  val draw  = Outcome(None)

  lazy val knownResultStrings = "*" :: normalizationMap.keys.toList

  type Points     = 0 | 0.5 | 1
  type GamePoints = ByColor[Points]

  def fromPoints(points: ByColor[Points]): Option[Outcome] = points match
    case ByColor(1, 0)     => Some(white)
    case ByColor(0, 1)     => Some(black)
    case ByColor(0.5, 0.5) => Some(draw)
    case _                 => None

  def pointsFromResult(result: String): Option[GamePoints] =
    normalizationMap.get(result)

  private val normalizationMap: Map[String, GamePoints] =
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
    val allCancels = for
      separator <- separators
      loss      <- losses
    yield s"$loss$separator$loss"
    val allHalfWins = for
      separator <- separators
      loss      <- losses
      draw      <- draws
    yield s"$draw$separator$loss"
    val allHalfLosses = for
      separator <- separators
      loss      <- losses
      draw      <- draws
    yield s"$loss$separator$draw"

    val pairs = allDraws.map(_ -> ByColor[Points](0.5, 0.5)) :::
      allWins.map(_ -> ByColor[Points](1, 0)) :::
      allLosses.map(_ -> ByColor[Points](0, 1)) :::
      allCancels.map(_ -> ByColor[Points](0, 0)) :::
      allHalfWins.map(_ -> ByColor[Points](0.5, 0)) :::
      allHalfLosses.map(_ -> ByColor[Points](0, 0.5))

    val lccResults = Map(
      "WHITEWIN" -> ByColor[Points](1, 0),
      "BLACKWIN" -> ByColor[Points](0, 1),
      "DRAW"     -> ByColor[Points](0.5, 0.5) // ? not sure
    )

    pairs.toMap ++ lccResults
