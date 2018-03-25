package chess

final case class LagTracker(
    quotaGain: Centis,
    quota: Centis,
    quotaMax: Centis,
    lagEstimator: DecayingRecorder,
    totalComp: Centis = Centis(0),
    lagStats: Stats = EmptyStats,
    // We can remove compEst fields after tuning estimate.
    compEstSqErr: Int = 0,
    compEstOvers: Centis = Centis(0),
    compEstimate: Option[Centis] = None
) {
  def onMove(lag: Centis) = {
    val comp = lag atMost quota
    val ceDiff = compEstimate.getOrElse(Centis(1)) - comp

    (comp, copy(
      quota = (quota + quotaGain - comp) atMost quotaMax,
      totalComp = totalComp + comp,
      lagStats = lagStats record (lag atMost Centis(2000)).centis,
      compEstSqErr = compEstSqErr + ceDiff.centis * ceDiff.centis,
      compEstOvers = compEstOvers + ceDiff.nonNeg
    ).recordLag(lag))
  }

  def recordLag(lag: Centis) = {
    val e = lagEstimator.record((lag atMost quotaMax).centis)
    copy(
      lagEstimator = e,
      compEstimate = Some {
        Centis(e.mean - .6f * e.deviation).nonNeg atMost quota
      }
    )
  }

  def moves = lagStats.samples

  def lagMean: Option[Centis] = moves > 0 option Centis(lagStats.mean)

  def compEstStdErr: Option[Float] =
    moves > 2 option Math.sqrt(compEstSqErr).toFloat / (moves - 2)

  def compAvg: Option[Centis] = totalComp / moves

  def totalLag: Centis = Centis(lagStats.total)

  def totalUncomped = totalLag - totalComp
}

object LagTracker {
  def init(config: Clock.Config) = {
    val quotaGain = Centis(config.estimateTotalSeconds match {
      case i if i >= 136 => 100
      case i if i <= 15 => 35
      case i => i / 2 + 32
    })
    LagTracker(
      quotaGain = quotaGain,
      quota = quotaGain * 3,
      quotaMax = quotaGain * 6,
      lagEstimator = EmptyDecayingStats(deviation = 6f, decay = 0.9f)
    )
  }
}

