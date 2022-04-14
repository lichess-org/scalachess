package chess

// Welford's numerically stable online variance.
final case class Stats(samples: Int, mean: Float, sn: Float) {

  def record(value: Float) = {
    val newSamples = samples + 1
    val delta      = value - mean
    val newMean    = mean + delta / newSamples
    val newSN      = sn + delta * (value - newMean)

    Stats(
      samples = newSamples,
      mean = newMean,
      sn = newSN
    )
  }

  def record[T](values: Iterable[T])(using n: Numeric[T]): Stats =
    values.foldLeft(this) { (s, v) =>
      s record n.toFloat(v)
    }

  def variance = (samples > 1) option sn / (samples - 1)

  def stdDev = variance.map { Math.sqrt(_).toFloat }

  def total = samples * mean
}

object Stats {
  val empty: Stats                                  = Stats(0, 0, 0)
  def apply(value: Float): Stats                    = empty record value
  def apply[T: Numeric](values: Iterable[T]): Stats = empty record values
}
