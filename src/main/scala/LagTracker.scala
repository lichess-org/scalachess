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
      val c = h.mean - Math.max(h.deviation, 2f)
      Some(Centis(c.toInt).nonNeg atMost quota)
    }
    case _ => None
  }
}

object LagTracker {
  def init(config: Clock.Config) = {
    val quotaGain = Centis(config.estimateTotalSeconds max 50 min 100)
    LagTracker(
      quotaGain = quotaGain,
      quota = quotaGain * 2,
      quotaMax = quotaGain * 5,
      history = EmptyDecayingStats(deviation = 5f, decay = 0.9f)
    )
  }
}

