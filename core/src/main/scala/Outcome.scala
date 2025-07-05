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

  enum Points:
    case Zero, Half, One

    def value: Float = this match
      case Zero => 0f
      case Half => 0.5f
      case One  => 1f

    def show: String = this match
      case Zero => "0"
      case Half => "1/2"
      case One  => "1"

  import Points.*
  type GamePoints = ByColor[Points]

  def fromPoints(points: ByColor[Points]): Option[Outcome] = points match
    case ByColor(One, Zero)  => Some(white)
    case ByColor(Zero, One)  => Some(black)
    case ByColor(Half, Half) => Some(draw)
    case _                   => None

  def pointsFromResult(result: String): Option[GamePoints] =
    normalizationMap.get(result)

  def outcomeToPoints(outcome: Outcome): GamePoints = outcome match
    case Outcome(Some(White)) => ByColor(One, Zero)
    case Outcome(Some(Black)) => ByColor(Zero, One)
    case Outcome(None)        => ByColor(Half, Half)

  def showPoints(points: Option[GamePoints]): String =
    points.fold("*"):
      _.mapList(_.show).mkString("-")

  def guessPointsFromStatusAndPosition(status: Status, positionWinner: Option[Color]): Option[GamePoints] =
    import Status.*
    status match
      case Created | Started                                                => None
      case Aborted | Cheat | NoStart                                        => Some(ByColor(Zero, Zero))
      case Stalemate | Draw                                                 => Some(ByColor(Half, Half))
      case Mate | Resign | Timeout | Outoftime | UnknownFinish | VariantEnd =>
        positionWinner match
          case Some(White) => Some(ByColor(One, Zero))
          case Some(Black) => Some(ByColor(Zero, One))
          case None        => Some(ByColor(Half, Half))

  private val normalizationMap: Map[String, GamePoints] =
    val hyphen     = "-"
    val enDash     = "–"
    val emDash     = "—"
    val dashes     = List(hyphen, enDash, emDash)
    val separators = dashes ::: List("_", ":")
    val draws      = List("½", "1/2", "0.5")
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

    val pairs = allDraws.map(_ -> ByColor[Points](Half, Half)) :::
      allWins.map(_ -> ByColor[Points](One, Zero)) :::
      allLosses.map(_ -> ByColor[Points](Zero, One)) :::
      allCancels.map(_ -> ByColor[Points](Zero, Zero)) :::
      allHalfWins.map(_ -> ByColor[Points](Half, Zero)) :::
      allHalfLosses.map(_ -> ByColor[Points](Zero, Half))

    val lccResults = Map(
      "WHITEWIN" -> ByColor[Points](One, Zero),
      "BLACKWIN" -> ByColor[Points](Zero, One),
      "DRAW"     -> ByColor[Points](Half, Half) // ? not sure
    )

    pairs.toMap ++ lccResults
