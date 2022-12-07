package chess

opaque type Timestamp = Long
object Timestamp extends OpaqueLong[Timestamp]:
  extension (t: Timestamp)
    def -(o: Timestamp): Centis = Centis.ofMillis(t - o.value)
    def +(o: Centis): Timestamp = Timestamp(t + o.millis)

trait Timestamper:
  def now: Timestamp
  def toNow(ts: Timestamp) = now - ts

private[chess] object RealTimestamper extends Timestamper:
  def now = Timestamp(System.currentTimeMillis)
