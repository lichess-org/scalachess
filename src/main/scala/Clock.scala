package chess

import scala.concurrent.duration._

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

  def remainingTime(c: Color) = math.max(0, limit - elapsedTime(c))

  def remainingTimes = Color.all map { c ⇒ c -> remainingTime(c) } toMap

  def elapsedTime(c: Color) = time(c)

  def limitInMinutes = limit / 60

  def estimateTotalTime = limit + 30 * increment

  // if lag is provided, it is added to the clock as a compensation
  def step(lag: FiniteDuration = 0.millis): RunningClock

  def run: RunningClock

  def stop: PausedClock

  def addTime(c: Color, t: Float): Clock

  def giveTime(c: Color, t: Float): Clock

  def show = limitInMinutes.toString + " + " + increment.toString

  def isRunning = timerOption.isDefined

  def switch: Clock

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
    timer: Double) extends Clock {

  val timerOption = Some(timer)

  override def elapsedTime(c: Color) = time(c) + {
    if (c == color) now - timer else 0
  }.toFloat

  def step(lag: FiniteDuration = 0.millis) = {
    val t = now
    val lagSeconds = lag.toMillis.toFloat / 1000
    val lagCompensation = math.max(
      0,
      math.min(
        lagSeconds.pp - Clock.naturalLag,
        Clock.maxLagToCompensate)).pp
    addTime(
      color,
      math.max(0, (t - timer).toFloat - lagCompensation) - increment
    ).copy(
        color = !color,
        timer = t
      )
  }

  def run = this

  def stop = PausedClock(
    limit = limit,
    increment = increment,
    color = color,
    whiteTime = whiteTime + (if (color == White) (now - timer).toFloat else 0),
    blackTime = blackTime + (if (color == Black) (now - timer).toFloat else 0))

  def addTime(c: Color, t: Float): RunningClock = c match {
    case White ⇒ copy(whiteTime = whiteTime + t)
    case Black ⇒ copy(blackTime = blackTime + t)
  }

  def giveTime(c: Color, t: Float): RunningClock = addTime(c, -t)

  def switch: RunningClock = copy(color = !color)
}

case class PausedClock(
    limit: Int,
    increment: Int,
    color: Color,
    whiteTime: Float,
    blackTime: Float) extends Clock {

  val timerOption = None

  def step(lag: FiniteDuration = 0.millis) = run step lag

  def stop = this

  def addTime(c: Color, t: Float): PausedClock = c match {
    case White ⇒ copy(whiteTime = whiteTime + t)
    case Black ⇒ copy(blackTime = blackTime + t)
  }

  def giveTime(c: Color, t: Float): PausedClock = addTime(c, -t)

  def switch: PausedClock = copy(color = !color)

  def run = RunningClock(
    color = color,
    whiteTime = whiteTime,
    blackTime = blackTime,
    increment = increment,
    limit = limit,
    timer = now)
}

object Clock {

  val minInitLimit = 2
  // no more than this time will be offered to the lagging player
  val maxLagToCompensate = 0.7f
  // substracted from lag compensation
  val naturalLag = 0.1f

  def apply(
    limit: Int,
    increment: Int): PausedClock = PausedClock(
    limit = math.max(minInitLimit, limit),
    increment = increment,
    color = White,
    whiteTime = 0f,
    blackTime = 0f)

  def timeString(time: Int) = periodFormatter.print(
    org.joda.time.Duration.standardSeconds(time).toPeriod
  )

  private val periodFormatter = new org.joda.time.format.PeriodFormatterBuilder().
    printZeroAlways.
    minimumPrintedDigits(1).appendHours.appendSeparator(":").
    minimumPrintedDigits(2).appendMinutes.appendSeparator(":").
    appendSeconds.
    toFormatter
}
