package chess

case class LagTracker(
    quotaGain: Centis,
    quota: Centis,
    quotaMax: Centis,
    history: Option[DecayingStats] = None
) {
  def onMove(lag: Centis) = {
    val lagComp = lag.nonNeg atMost quota

    (lagComp, copy(
      quota = (quota + quotaGain - lagComp) atMost quotaMax,
      history = Some(history.fold(
        DecayingStats(
          mean = lagComp.centis,
          deviation = 5f,
          decay = 0.9f
        )
      ) { _.record(lagComp.centis) })
    ))
  }

  def estimate = history.map { h => Centis(h.mean.toInt) }

  def lowEstimate = history.map { h =>
    {
      val c = h.mean - Math.max(h.deviation, 2f)
      Centis(c.toInt max 0) atMost quota
    }
  }
}

object LagTracker {
  def forClock(config: Clock.Config) = {
    val quotaGain = Centis(config.estimateTotalSeconds max 50 min 100)
    LagTracker(
      quotaGain = quotaGain,
      quota = quotaGain * 2,
      quotaMax = quotaGain * 5
    )
  }
}

