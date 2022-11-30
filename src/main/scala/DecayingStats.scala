package chess

trait DecayingRecorder:
  def record(value: Float): DecayingStats

case class DecayingStats(
    mean: Float,
    deviation: Float,
    decay: Float
) extends DecayingRecorder:
  def record(value: Float): DecayingStats =
    val delta = mean - value
    copy(
      mean = value + decay * delta,
      deviation = decay * deviation + (1 - decay) * Math.abs(delta)
    )

  def record[T](values: Iterable[T])(using n: Numeric[T]): DecayingStats =
    values.foldLeft(this) { (s, v) =>
      s record n.toFloat(v)
    }

object DecayingStats:
  val empty = new DecayingRecorder:
    def record(value: Float) =
      DecayingStats(
        mean = value,
        deviation = 4f,
        decay = 0.85f
      )
