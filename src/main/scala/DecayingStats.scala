package chess

final case class DecayingStats(
    mean: Float,
    variance: Float,
    decay: Float
) {
  def record[T](values: Traversable[T])(implicit n: Numeric[T]): DecayingStats =
    values.foldLeft(this) { (s, v) => s record n.toFloat(v) }

  def record(value: Float) = {
    val delta = mean - value

    copy(
      mean = value + decay * delta,
      variance = decay * variance + (1 - decay) * delta * delta
    )
  }

  def stdDev = Math.sqrt(variance).toFloat
}

object DecayingStats {
  def empty(baseVarience: Float, decay: Float = 0.9f)(value: Float) =
    new DecayingStats(
      mean = value,
      variance = baseVarience + 0.5f * value * value,
      decay = decay
    )
}