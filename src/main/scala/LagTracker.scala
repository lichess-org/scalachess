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
        DecayingStats(
          mean = lagComp.centis,
          variance = 150f,
          decay = 0.9f,
          decayVar = 0.8f
        )
      ) { _.record(lagComp.centis) })
    ))
  }

  def estimate = history.map { h => Centis(h.mean.toInt) }

  def lowEstimate = history.map { h =>
    {
      val c = h.mean - Math.max(h.stdDev, 2f)
      Centis(c.toInt max 0) atMost quota
    }
  }
}

object LagTracker {
  val quotaGain = Centis(100)
  val quotaMax = Centis(500)
  val maxMoveComp = Centis(300)
}

