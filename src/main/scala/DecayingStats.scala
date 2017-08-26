package chess

final case class DecayingStats(
    mean: Float,
    deviation: Float,
    decay: Float
) {
  def record[T](values: Traversable[T])(implicit n: Numeric[T]): DecayingStats =
    values.foldLeft(this) { (s, v) => s record n.toFloat(v) }

  def record(value: Float) = {
    val delta = mean - value

    copy(
      mean = value + decay * delta,
      deviation = decay * deviation + (1 - decay) * math.abs(delta)
    )
  }
}

object DecayingStats {
  def empty(deviation: Float, decay: Float = 0.9f)(value: Float) =
    DecayingStats(
      mean = value,
      deviation = deviation,
      decay = decay
    )
}
