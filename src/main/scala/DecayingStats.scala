package chess

final case class DecayingStats(
    mean: Float,
    variance: Float,
    decay: Float,
    decayVar: Float
) {
  def record[T](values: Traversable[T])(implicit n: Numeric[T]): DecayingStats =
    values.foldLeft(this) { (s, v) => s record n.toFloat(v) }

  def record(value: Float) = {
    val delta = mean - value

    copy(
      mean = value + decay * delta,
      variance = decayVar * variance + (1 - decayVar) * delta * delta
    )
  }

  def stdDev = Math.sqrt(variance).toFloat
}

object DecayingStats {
  def empty(variance: Float, decay: Float = 0.9f)(value: Float) =
    DecayingStats(
      mean = value,
      variance = variance,
      decay = decay,
      decayVar = decay * Math.sqrt(decay).toFloat
    )
}
