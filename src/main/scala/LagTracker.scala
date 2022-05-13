package chess

final case class LagTracker(
    quotaGain: Centis,
    quota: Centis,
    quotaMax: Centis,
    lagEstimator: DecayingRecorder,
    uncompStats: Stats = Stats.empty,
    lagStats: Stats = Stats.empty,
    // We can remove compEst fields after tuning estimate.
    compEstSqErr: Int = 0,
    compEstOvers: Centis = Centis(0),
    compEstimate: Option[Centis] = None
) {

  def onMove(lag: Centis) = {
    val comp     = lag atMost quota
    val uncomped = lag - comp
    val ceDiff   = compEstimate.getOrElse(Centis(1)) - comp

    (
      comp,
      copy(
        quota = quota + quotaGain - comp atMost quotaMax,
        uncompStats = {
          // start recording after first uncomp.
          if (uncomped == Centis(0) && uncompStats.samples == 0) uncompStats
          else uncompStats record uncomped.centis.toFloat
        },
        lagStats = lagStats record (lag atMost Centis(2000)).centis.toFloat,
        compEstSqErr = compEstSqErr + ceDiff.centis * ceDiff.centis,
        compEstOvers = compEstOvers + ceDiff.nonNeg
      ).recordLag(lag)
    )
  }

  def recordLag(lag: Centis) = {
    val e = lagEstimator.record((lag atMost quotaMax).centis.toFloat)
    copy(
      lagEstimator = e,
      compEstimate = Option(Centis(e.mean - .8f * e.deviation).nonNeg atMost quota)
    )
  }

  def moves = lagStats.samples

  def lagMean: Option[Centis] = moves > 0 option Centis(lagStats.mean)

  def compEstStdErr: Option[Float] =
    moves > 2 option Math.sqrt(compEstSqErr).toFloat / (moves - 2)

  def compAvg: Option[Centis] = totalComp / moves

  def totalComp: Centis = totalLag - totalUncomped

  def totalLag: Centis = Centis(lagStats.total)

  def totalUncomped: Centis = Centis(uncompStats.total)

  def withFrameLag(frameLag: Centis, clock: Clock.Config) = copy(
    quotaGain = LagTracker.maxQuotaGainFor(clock).atMost {
      frameLag + LagTracker.estimatedCpuLag
    }
  )
}

object LagTracker {

  private val estimatedCpuLag = Centis(4)

  def maxQuotaGainFor(config: Clock.Config) = Centis(config.estimateTotalSeconds match {
    case i if i >= 180 => 100
    case i if i <= 15  => 20
    case i if i <= 30  => 35
    case i             => i / 4 + 30
  })

  def init(config: Clock.Config) = {
    val quotaGain = maxQuotaGainFor(config)
    LagTracker(
      quotaGain = quotaGain,
      quota = quotaGain * 3,
      quotaMax = quotaGain * 7,
      lagEstimator = EmptyDecayingStats(deviation = 4f, decay = 0.85f)
    )
  }

}
