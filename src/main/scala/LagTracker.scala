package chess

final case class LagTracker(
    quotaGain: Centis,
    quota: Centis,
    quotaMax: Centis,
    history: DecayingRecorder,
    totalComp: Centis = Centis(0),
    totalLag: Centis = Centis(0),
    lagSteps: Int = 0
) {
  def onMove(lag: Centis) = {
    val comp = lag atMost quota

    (comp, copy(
      quota = (quota + quotaGain - comp) atMost quotaMax,
      history = history.record(comp.centis),
      totalComp = totalComp + comp,
      totalLag = totalLag + lag,
      lagSteps = lagSteps + 1
    ))
  }

  def recordLag(lag: Centis) =
    copy(history = history.record((lag atMost quota).centis))

  def avgLagComp = totalComp / lagSteps

  def avgLag = totalLag / lagSteps

  def lowEstimate = history match {
    case h: DecayingStats => {
      val c = h.mean - .8f * h.deviation
      Some(Centis(Math.round(c)).nonNeg atMost quota)
    }
    case _ => None
  }
}

object LagTracker {
  def init(config: Clock.Config) = {
    val quotaGain = Centis(config.estimateTotalSeconds match {
      case i if i >= 140 => 100
      case i if i <= 20 => 40
      case i => i / 2 + 30
    })
    LagTracker(
      quotaGain = quotaGain,
      quota = quotaGain * 3,
      quotaMax = quotaGain * 6,
      history = EmptyDecayingStats(deviation = 2f, decay = 0.9f)
    )
  }
}

