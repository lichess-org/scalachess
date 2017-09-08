package chess

trait DecayingRecorder {
  def record(value: Float): DecayingStats
}

final case class DecayingStats(
    mean: Float,
    deviation: Float,
    decay: Float
) extends DecayingRecorder {
  def record(value: Float): DecayingStats = {
    val delta = mean - value

    copy(
      mean = value + decay * delta,
      deviation = decay * deviation + (1 - decay) * math.abs(delta)
    )
  }

  def record[T](values: Traversable[T])(implicit n: Numeric[T]): DecayingStats =
    values.foldLeft(this) { (s, v) => s record n.toFloat(v) }
}

object DecayingStats {
  def empty(deviation: Float, decay: Float) = new DecayingRecorder {
    def record(value: Float) = DecayingStats(
      mean = value,
      deviation = deviation,
      decay = decay
    )
  }
}
