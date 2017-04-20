package chess

case class Timestamp(value: Long) extends AnyVal with Ordered[Timestamp] {
  // Convert to centi duration with simple int round.
  def to(end: Timestamp) =
    if (end.value > value) Centis((end.value - value + 5) / 10)
    else Centis((end.value - value - 4) / 10)

  def compare(other: Timestamp) = value compare other.value
}

object Timestamp {
  def now = Timestamp(System.currentTimeMillis)
}