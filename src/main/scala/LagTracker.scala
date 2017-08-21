package chess

import LagTracker._

case class LagTracker(
    quotaGain: Centis,
    quota: Centis = Centis(200),
    history: Option[DecayingStats] = None
) {
  def onMove(lag: Centis) = {
    val lagComp = lag.nonNeg atMost quota

    (lagComp, copy(
      quota = (quota + quotaGain - lagComp) atMost quotaMax,
      history = Some(history.fold(
        DecayingStats(
          mean = lagComp.centis,
          variance = 20f,
          decay = 0.9f,
          decayVar = 0.8f
        )
      ) { _.record(lagComp.centis) })
    ))
  }

  def estimate = history.map { h => Centis(h.mean.toInt) }

  def lowEstimate = history.map { h =>
    {
      val c = h.mean - Math.max(1.5f * h.stdDev, 2f)
      Centis(c.toInt max 0) atMost quota
    }
  }
}

object LagTracker {
  val quotaMax = Centis(500)

  def forClock(secs: Int) = LagTracker(
    quotaGain = Centis(secs max 50 min 100)
  )
}

