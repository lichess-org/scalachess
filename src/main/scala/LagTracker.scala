package chess

import LagTracker._

case class LagTracker(
    quota: Centis = Centis(200),
    history: Option[DecayingStats] = None
) {

  @inline def maxNextComp = maxMoveComp atMost quota

  def onMove(lag: Centis) = {
    val lagComp = lag.nonNeg atMost maxNextComp

    val recorder = history getOrElse initialHistory

    (lagComp, copy(
      quota = (quota + quotaGain - lagComp) atMost quotaMax,
      history = Some(recorder.record(lagComp.centis))
    ))
  }

  def estimate = history.map { h => Centis(h.mean.toInt) }

  def lowEstimate = history.map { h =>
    Centis((h.mean - h.stdDev).toInt).nonNeg
  }
}

object LagTracker {
  val quotaGain = Centis(100)
  val quotaMax = Centis(500)
  val maxMoveComp = Centis(300)
  private val initialHistory = DecayingStats.empty(baseVarience = 10 * 10)
}

