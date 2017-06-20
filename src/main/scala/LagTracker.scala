package chess

import LagTracker._

case class LagTracker(
    quota: Centis = Centis(200),
    history: Option[DecayingStats] = None
) {

  @inline def maxNextComp = maxMoveComp atMost quota

  def onMove(lag: Centis) = {
    val lagComp = lag.nonNeg atMost maxNextComp

    (lagComp, copy(
      quota = (quota + quotaGain - lagComp) atMost quotaMax,
      history = Some(history.fold(
        DecayingStats.empty(baseVariance = 500)(lagComp.centis)
      )(_.record(lagComp.centis)))
    ))
  }

  def estimate = history.map { h => Centis(h.mean.toInt) }

  def lowEstimate = history.map { h =>
    {
      val c = h.mean - Math.max(0.5f * h.stdDev, 10f)
      Centis(c.toInt max 0) atMost quota
    }
  }
}

object LagTracker {
  val quotaGain = Centis(100)
  val quotaMax = Centis(500)
  val maxMoveComp = Centis(300)
}

