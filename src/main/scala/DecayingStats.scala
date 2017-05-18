package chess
sealed trait DecayingStats {
  def mean: Float
  def variance: Float
  def decay: Float

  def stdDev = Math.sqrt(variance).toFloat

  def record[T](values: Traversable[T])(implicit n: Numeric[T]): DecayingStats =
    values.foldLeft(this) { (s, v) => s record n.toFloat(v) }

  def record(value: Float): DecayingStats
}

private[chess] final case class DecayingStatsHolder(
    mean: Float,
    variance: Float,
    decay: Float
) extends DecayingStats {

  def record(value: Float) = {
    val delta = mean - value

    new DecayingStatsHolder(
      mean = value + decay * delta,
      variance = decay * variance + (1 - decay) * delta * delta,
      decay = decay
    )
  }
}

private[chess] final case class EmptyDecayingStats(
    variance: Float,
    decay: Float = 0.9f
) extends DecayingStats {
  val mean = Float.NaN
  def record(value: Float) = new DecayingStatsHolder(
    mean = value,
    variance = decay * variance + (1 - decay) * value * value,
    decay = decay
  )
}

object DecayingStats {
  def empty = EmptyDecayingStats
}

