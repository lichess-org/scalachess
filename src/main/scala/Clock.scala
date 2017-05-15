package chess

import java.text.DecimalFormat

// All unspecified durations are expressed in seconds
sealed trait Clock {
  @inline implicit def CentisWrapper(c: Int) = new Centis(c)
  @inline implicit def CentisWrapper(c: Long) = Centis(c)

  val config: Clock.Config
  val color: Color
  val whiteTime: Centis
  val blackTime: Centis
  val whiteBerserk: Boolean
  val blackBerserk: Boolean
  val timerOption: Option[Timestamp]
  val timestamper: Timestamper

  def limitSeconds = config.limitSeconds
  def limit = config.limit

  def incrementSeconds = config.incrementSeconds
  def increment = config.increment

  def time(c: Color): Centis = c.fold(whiteTime, blackTime)

  def outoftime(c: Color) = remainingTime(c).centis == 0

  def outoftimeWithGrace(c: Color, grace: Centis) =
    timeSinceFlag(c).exists((grace atMost Clock.maxGrace).<)

  def remainingTime(c: Color): Centis = (limit - elapsedTime(c)) nonNeg

  def incrementOf(c: Color): Centis =
    if (c.fold(whiteBerserk, blackBerserk)) 0 else increment

  def setRemainingTime(c: Color, centis: Centis) =
    addTime(c, remainingTime(c) - centis)

  private def timeSinceFlag(c: Color): Option[Centis] = (limit - elapsedTime(c)) match {
    case s if s.centis <= 0 => Some(-s)
    case _ => None
  }

  def remainingTimes = Map(White -> remainingTime(White), Black -> remainingTime(Black))

  def elapsedTime(c: Color) = time(c)

  def limitInMinutes = config.limitInMinutes

  def estimateTotalIncrement = config.estimateTotalIncrement

  def estimateTotalTime = config.estimateTotalTime

  def estimateTotalSeconds = config.estimateTotalSeconds

  // Emergency time cutoff, in seconds.
  def emergTime = config.emergTime

  def stop: PausedClock

  def addTime(c: Color, t: Centis): Clock

  def giveTime(c: Color, t: Centis): Clock

  def berserked(c: Color) = c.fold(whiteBerserk, blackBerserk)

  def goBerserk(c: Color): Clock

  def show = config.show

  def moretimeable(c: Color) = remainingTime(c).centis < 100 * 60 * 60 * 2

  def isRunning = timerOption.isDefined

  def isInit = elapsedTime(White).centis == 0 && elapsedTime(Black).centis == 0

  def switch: Clock

  def takeback: Clock

  def reset = Clock(config)
}

case class RunningClock(
    config: Clock.Config,
    color: Color,
    whiteTime: Centis,
    blackTime: Centis,
    whiteBerserk: Boolean,
    blackBerserk: Boolean,
    timer: Timestamp,
    timestamper: Timestamper = RealTimestamper
) extends Clock {
  import timestamper.{ now, toNow }

  val timerOption = Some(timer)

  override def elapsedTime(c: Color) = {
    if (c == color) toNow(timer) + time(c) else time(c)
  }

  def step(lag: Centis = 0, withInc: Boolean = true) = {
    val t = now
    val lagComp: Centis = lag atMost Clock.maxLagToCompensate nonNeg
    val inc: Centis = if (withInc) incrementOf(color) else 0
    addTime(
      color,
      (((t - timer) - lagComp) nonNeg) - inc
    ).copy(
        color = !color,
        timer = t
      )
  }

  def stop = PausedClock(
    config = config,
    color = color,
    whiteTime = color.fold(whiteTime + toNow(timer), whiteTime),
    blackTime = color.fold(blackTime, blackTime + toNow(timer)),
    whiteBerserk = whiteBerserk,
    blackBerserk = blackBerserk,
    timestamper = timestamper
  )

  def addTime(c: Color, t: Centis): RunningClock = c match {
    case White => copy(whiteTime = whiteTime + t)
    case Black => copy(blackTime = blackTime + t)
  }

  def giveTime(c: Color, t: Centis): RunningClock = addTime(c, -t)

  def goBerserk(c: Color): RunningClock = addTime(c, config.berserkPenalty).copy(
    whiteBerserk = whiteBerserk || c.white,
    blackBerserk = blackBerserk || c.black
  )

  def switch: RunningClock = copy(color = !color)

  def takeback: RunningClock = {
    copy(
      color = !color,
      timer = now
    )
  }
}

case class PausedClock(
    config: Clock.Config,
    color: Color,
    whiteTime: Centis,
    blackTime: Centis,
    whiteBerserk: Boolean,
    blackBerserk: Boolean,
    timestamper: Timestamper = RealTimestamper
) extends Clock {

  val timerOption = None

  def stop = this

  def addTime(c: Color, t: Centis): PausedClock = c match {
    case White => copy(whiteTime = whiteTime + t)
    case Black => copy(blackTime = blackTime + t)
  }

  def giveTime(c: Color, t: Centis): PausedClock = addTime(c, -t)

  def goBerserk(c: Color): PausedClock = addTime(c, config.berserkPenalty).copy(
    whiteBerserk = c.fold(true, whiteBerserk),
    blackBerserk = c.fold(blackBerserk, true)
  )

  def switch: PausedClock = copy(color = !color)

  def takeback: PausedClock = switch

  def start = RunningClock(
    config = config,
    color = color,
    whiteTime = whiteTime,
    blackTime = blackTime,
    whiteBerserk = whiteBerserk,
    blackBerserk = blackBerserk,
    timer = timestamper.now,
    timestamper = timestamper
  )
}

object Clock {
  private val limitFormatter = new DecimalFormat("#.##")

  // All unspecified durations are expressed in seconds
  case class Config(limitSeconds: Int, incrementSeconds: Int) {

    def limitInMinutes = limitSeconds / 60d

    def limit = Centis.ofSeconds(limitSeconds)

    def estimateTotalIncrement = Centis.ofSeconds(estimateTotalIncSeconds)

    def estimateTotalIncSeconds = 40 * incrementSeconds

    def estimateTotalTime = Centis.ofSeconds(estimateTotalSeconds)

    def estimateTotalSeconds = limitSeconds + estimateTotalIncSeconds

    // Emergency time cutoff, in seconds. 
    def emergTime = math.min(60, math.max(10, limitSeconds / 8))

    def hasIncrement = incrementSeconds > 0

    def increment = Centis.ofSeconds(incrementSeconds)

    def berserkable = incrementSeconds == 0 || limitSeconds > 0

    def toClock = Clock(this)

    def limitString = limitSeconds match {
      case l if l % 60 == 0 => l / 60
      case 15 => "¼"
      case 30 => "½"
      case 45 => "¾"
      case 90 => "1.5"
      case _ => limitFormatter.format(limitSeconds / 60d)
    }

    def show = toString

    override def toString = s"$limitString+$incrementSeconds"

    def berserkPenalty =
      if (limitSeconds < 40 * incrementSeconds) Centis(0)
      else Centis(limitSeconds * (100 / 2))
  }

  // [TimeControl "600+2"] -> 10+2
  def readPgnConfig(str: String): Option[Config] = str.split('+') match {
    case Array(initStr, incStr) => for {
      init <- parseIntOption(initStr)
      inc <- parseIntOption(incStr)
    } yield Config(init, inc)
    case _ => none
  }

  val minInitLimit = Centis(300)
  // no more than this time will be offered to the lagging player
  val maxLagToCompensate = Centis(100)
  // no more than this time to get the last move in
  val maxGrace = Centis(100)

  def apply(limit: Int, increment: Int): PausedClock = apply(Config(limit, increment))

  def apply(config: Config): PausedClock = {
    PausedClock(
      config = config,
      color = White,
      whiteTime = Centis(0),
      blackTime = Centis(0),
      whiteBerserk = false,
      blackBerserk = false
    )
  }
}
