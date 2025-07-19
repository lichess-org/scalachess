package chess

import chess.Clock.Config

import java.text.DecimalFormat

// All unspecified durations are expressed in seconds
case class Clock(
    config: Config,
    color: Color,
    players: ByColor[ClockPlayer],
    timer: Option[Timestamp] = None,
    timestamper: Timestamper = RealTimestamper
):
  import timestamper.{ now, toNow }

  export config.{
    estimateTotalSeconds,
    estimateTotalTime,
    increment,
    incrementSeconds,
    limit,
    limitInMinutes,
    limitSeconds
  }

  inline def timerFor(c: Color) = if c == color then timer else None

  inline def pending(c: Color) = timerFor(c).fold(Centis(0))(toNow)

  def remainingTime(c: Color) = (players(c).remaining - pending(c)).nonNeg

  def outOfTime(c: Color, withGrace: Boolean) =
    players(c).remaining <=
      timerFor(c).fold(Centis(0)) { t =>
        if withGrace then (toNow(t) - (players(c).lag.quota.atMost(Centis(200)))).nonNeg
        else toNow(t)
      }

  def moretimeable(c: Color) = players(c).remaining < Centis(100 * 60 * 60 * 2)

  def isRunning = timer.isDefined

  def start = if isRunning then this else copy(timer = Option(now))

  def stop =
    timer.fold(this) { t =>
      copy(
        players = players.update(color, _.takeTime(toNow(t))),
        timer = None
      )
    }

  def hardStop = copy(timer = None)

  def updatePlayer(c: Color)(f: ClockPlayer => ClockPlayer) =
    copy(players = players.update(c, f))

  def switch =
    copy(
      color = !color,
      timer = timer.map(_ => now)
    )

  def withFrameLag(frameLag: Centis) = updatePlayer(color)(_.withFrameLag(frameLag))

  def step(
      metrics: MoveMetrics = MoveMetrics.empty,
      gameActive: Boolean = true
  ): Clock.WithCompensatedLag[Clock] =
    timer match
      case None =>
        Clock.WithCompensatedLag(
          metrics.clientLag
            .fold(this) { l =>
              updatePlayer(color) { _.recordLag(l) }
            }
            .switch,
          None
        )
      case Some(t) =>
        val elapsed = toNow(t)
        val lag     = (~metrics.reportedLag(elapsed)).nonNeg

        val player              = players(color)
        val (lagComp, lagTrack) = player.lag.onMove(lag)

        val moveTime = (elapsed - lagComp).nonNeg

        val clockActive = gameActive && moveTime < player.remaining
        val inc         = clockActive.so(player.increment)

        val newC = updatePlayer(color):
          _.takeTime(moveTime - inc)
            .copy(lag = lagTrack)

        Clock.WithCompensatedLag(
          (if clockActive then newC else newC.hardStop).switch,
          Some(lagComp)
        )

  // To do: safely add this to takeback to remove inc from player.
  // def deinc = updatePlayer(color, _.giveTime(-incrementOf(color)))

  def takeback: Clock = switch

  def giveTime(c: Color, t: Centis): Clock =
    updatePlayer(c):
      _.giveTime(t)

  def setRemainingTime(c: Color, centis: Centis): Clock =
    updatePlayer(c):
      _.setRemaining(centis)

  def incrementOf(c: Color): Centis = players(c).increment

  def goBerserk(c: Color): Clock = updatePlayer(c)(_.copy(berserk = true))

  def berserked(c: Color): Boolean = players(c).berserk
  def lag(c: Color): LagTracker    = players(c).lag

  def lagCompAvg: Centis = players.mapReduce(~_.lag.compAvg)(_.avg(_))

  // Lowball estimate of next move's lag comp for UI butter.
  def lagCompEstimate(c: Color): Option[Centis] = players(c).lag.compEstimate

case class ClockPlayer(
    config: Clock.Config,
    lag: LagTracker,
    elapsed: Centis = Centis(0),
    berserk: Boolean = false
):
  def limit: Centis =
    if berserk then config.initTime - config.berserkPenalty
    else config.initTime

  def recordLag(l: Centis): ClockPlayer = copy(lag = lag.recordLag(l))

  def remaining: Centis = limit - elapsed

  def takeTime(t: Centis): ClockPlayer = copy(elapsed = elapsed + t)

  def giveTime(t: Centis): ClockPlayer = takeTime(-t)

  def setRemaining(t: Centis): ClockPlayer = copy(elapsed = limit - t)

  def increment: Centis = if berserk then Centis(0) else config.increment

  def withFrameLag(frameLag: Centis): ClockPlayer = copy(lag = lag.withFrameLag(frameLag, config))

object ClockPlayer:
  def withConfig(config: Clock.Config): ClockPlayer =
    ClockPlayer(config, LagTracker.init(config))

object Clock:
  private val limitFormatter: DecimalFormat = DecimalFormat("#.##")

  opaque type LimitSeconds = Int
  object LimitSeconds extends RelaxedOpaqueInt[LimitSeconds]

  opaque type LimitMinutes = Int
  object LimitMinutes extends RelaxedOpaqueInt[LimitMinutes]

  opaque type IncrementSeconds = Int
  object IncrementSeconds extends RelaxedOpaqueInt[IncrementSeconds]

  // All unspecified durations are expressed in centi-seconds
  case class Config(limitSeconds: LimitSeconds, incrementSeconds: IncrementSeconds):

    def berserkable: Boolean = incrementSeconds == 0 || limitSeconds > 0

    def emergSeconds: Int = math.min(60, math.max(10, limitSeconds / 8))

    def estimateTotalSeconds: Int = limitSeconds + 40 * incrementSeconds

    def estimateTotalTime: Centis = Centis.ofSeconds(estimateTotalSeconds)

    def hasIncrement: Boolean = incrementSeconds > 0

    def increment: Centis = Centis.ofSeconds(incrementSeconds)

    def limit: Centis = Centis.ofSeconds(limitSeconds)

    def limitInMinutes: Double = limitSeconds / 60d

    def toClock: Clock = Clock(this)

    def limitString: String =
      limitSeconds match
        case l if l % 60 == 0 => (l / 60).toString
        case 15 => "¼"
        case 30 => "½"
        case 45 => "¾"
        case 90 => "1.5"
        case _  => limitFormatter.format(limitSeconds / 60d)

    def show: String = toString

    override def toString = s"$limitString+$incrementSeconds"

    def berserkPenalty: Centis =
      if limitSeconds < 40 * incrementSeconds then Centis(0)
      else Centis(limitSeconds * (100 / 2))

    def initTime: Centis =
      if limitSeconds == 0 then increment.atLeast(Centis(300))
      else limit

  def apply(limit: LimitSeconds, increment: IncrementSeconds): Clock = apply(Config(limit, increment))

  def apply(config: Config): Clock =
    val player = ClockPlayer.withConfig(config)
    Clock(
      config = config,
      color = White,
      players = ByColor(player, player),
      timer = None
    )

  case class WithCompensatedLag[A](value: A, compensated: Option[Centis]):
    def map[B](f: A => B) = copy(value = f(value))
