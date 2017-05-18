package chess

import LagTracker._

case class LagTracker(
    quota: Centis = Centis(200),
    history: DecayingStats = initialHistory
) {

  def onMove(lag: Centis) = {
    val lagComp = lag.nonNeg atMost maxLagComp atMost quota

    (lagComp, copy(
      quota = (quota + quotaGain - lagComp) atMost quotaMax,
      history = history.record(lagComp.centis)
    ))
  }

  private def toCentis(f: Float) = if (f > 0) Centis(f.toInt) else Centis(0)

  def bestEstimate = toCentis(history.mean)
  def lowEstimate = toCentis(history.mean - history.stdDev)
}

object LagTracker {
  val quotaGain = Centis(100)
  val quotaMax = Centis(500)
  val maxLagComp = Centis(300)
  private val initialHistory = DecayingStats.empty(variance = 1e4f)
}

