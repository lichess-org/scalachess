package chess

import scala.concurrent.duration._

import java.text.DecimalFormat

// All durations are expressed in seconds
sealed trait Clock {
  val limit: Int
  val increment: Int
  val color: Color
  val whiteTime: Float
  val blackTime: Float
  val timerOption: Option[Double]

  def time(c: Color) = c.fold(whiteTime, blackTime)

  def outoftime(c: Color) = remainingTime(c) == 0

  def outoftimeWithGrace(c: Color, graceMillis: Int) =
    millisSinceFlag(c).exists(graceMillis.min(Clock.maxGraceMillis).<)

  def remainingTime(c: Color) = math.max(0, limit - elapsedTime(c))

  private def millisSinceFlag(c: Color): Option[Int] = (limit - elapsedTime(c)) match {
    case s if s <= 0 => Some((s * -1000).toInt)
    case _           => None
  }

  def remainingTimes = Color.all map { c => c -> remainingTime(c) } toMap

  def elapsedTime(c: Color) = time(c)

  def limitInMinutes = limit / 60d

  def estimateTotalIncrement = 40 * increment

  def estimateTotalTime = limit + estimateTotalIncrement

  def emergTime: Int = math.round(math.min(20, math.max(2, estimateTotalTime / 15)))

  def stop: PausedClock

  def addTime(c: Color, t: Float): Clock

  def giveTime(c: Color, t: Float): Clock

  def berserkable = increment == 0 || limit > 0

  def berserk(c: Color): Clock

  def show = s"${new DecimalFormat("#.##").format(limitInMinutes)}+$increment"

  def showTime(t: Float) = {
    val hours = math.floor(t / 3600).toInt
    val minutes = math.floor((t - hours * 3600) / 60).toInt
    val seconds = t.toInt % 60
    s"${if (hours > 0) hours else ""}:$minutes:$seconds"
  }

  def moretimeable(c: Color) = remainingTime(c) < 60 * 60 * 2

  def isRunning = timerOption.isDefined

  def isInit = elapsedTime(White) == 0 && elapsedTime(Black) == 0

  def switch: Clock

  def takeback: Clock

  def reset = Clock(
    limit = limit,
    increment = increment)

  protected def now = System.currentTimeMillis / 1000d
}

case class RunningClock(
    limit: Int,
    increment: Int,
    color: Color,
    whiteTime: Float,
    blackTime: Float,
    whiteBerserk: Boolean,
    blackBerserk: Boolean,
    timer: Double) extends Clock {

  val timerOption = Some(timer)

  override def elapsedTime(c: Color) = time(c) + {
    if (c == color) now - timer else 0
  }.toFloat

  def incrementOf(c: Color) =
    c.fold(whiteBerserk, blackBerserk).fold(0, increment)

  def step(lag: FiniteDuration = 0.millis) = {
    val t = now
    val spentTime = (t - timer).toFloat
    val lagSeconds = lag.toMillis.toFloat / 1000
    val lagCompensation = lagSeconds min Clock.maxLagToCompensate max 0
    addTime(
      color,
      (math.max(0, spentTime - lagCompensation) - incrementOf(color))
    ).copy(
        color = !color,
        timer = t)
  }

  def stop = PausedClock(
    limit = limit,
    increment = increment,
    color = color,
    whiteTime = whiteTime + (if (color == White) (now - timer).toFloat else 0),
    blackTime = blackTime + (if (color == Black) (now - timer).toFloat else 0),
    whiteBerserk = whiteBerserk,
    blackBerserk = blackBerserk)

  def addTime(c: Color, t: Float): RunningClock = c match {
    case White => copy(whiteTime = whiteTime + t)
    case Black => copy(blackTime = blackTime + t)
  }

  def giveTime(c: Color, t: Float): RunningClock = addTime(c, -t)

  def berserk(c: Color): RunningClock = addTime(c, Clock.berserkPenalty(this, color)).copy(
    whiteBerserk = c.fold(true, whiteBerserk),
    blackBerserk = c.fold(blackBerserk, true))

  def switch: RunningClock = copy(color = !color)

  def takeback: RunningClock = {
    val t = now
    val spentTime = (t - timer).toFloat
    addTime(color, spentTime).copy(
      color = !color,
      timer = t)
  }
}

case class PausedClock(
    limit: Int,
    increment: Int,
    color: Color,
    whiteTime: Float,
    blackTime: Float,
    whiteBerserk: Boolean,
    blackBerserk: Boolean) extends Clock {

  val timerOption = None

  def stop = this

  def addTime(c: Color, t: Float): PausedClock = c match {
    case White => copy(whiteTime = whiteTime + t)
    case Black => copy(blackTime = blackTime + t)
  }

  def giveTime(c: Color, t: Float): PausedClock = addTime(c, -t)

  def berserk(c: Color): PausedClock = addTime(c, Clock.berserkPenalty(this, color)).copy(
    whiteBerserk = c.fold(true, whiteBerserk),
    blackBerserk = c.fold(blackBerserk, true))

  def switch: PausedClock = copy(color = !color)

  def takeback: PausedClock = switch

  def start = RunningClock(
    color = color,
    whiteTime = whiteTime,
    blackTime = blackTime,
    whiteBerserk = whiteBerserk,
    blackBerserk = blackBerserk,
    increment = increment,
    limit = limit,
    timer = now)
}

object Clock {

  val minInitLimit = 3
  // no more than this time will be offered to the lagging player
  val maxLagToCompensate = 1f
  // no more than this time to get the last move in
  val maxGraceMillis = 800

  def apply(
    limit: Int,
    increment: Int): PausedClock = {
    val clock = PausedClock(
      limit = limit,
      increment = increment,
      color = White,
      whiteTime = 0f,
      blackTime = 0f,
      whiteBerserk = false,
      blackBerserk = false)
    if (clock.limit == 0) clock
      .giveTime(White, increment.max(minInitLimit))
      .giveTime(Black, increment.max(minInitLimit))
    else clock
  }

  private[chess] def berserkPenalty(clock: Clock, color: Color): Int = {
    val incTime = clock.estimateTotalIncrement
    val iniTime = clock.limit
    if (iniTime < incTime / 2) 0 else iniTime / 2
  }.toInt

  def timeString(t: Int) = periodFormatter.print(
    org.joda.time.Duration.standardSeconds(t).toPeriod
  )

  private val periodFormatter = new org.joda.time.format.PeriodFormatterBuilder().
    printZeroAlways.
    minimumPrintedDigits(1).appendHours.appendSeparator(":").
    minimumPrintedDigits(2).appendMinutes.appendSeparator(":").
    appendSeconds.
    toFormatter
}
