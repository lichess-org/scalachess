package chess

import scala.concurrent.duration._

import scalaz.Monoid
import ornicar.scalalib.Zero

// maximum centis = Int.MaxValue / 100 / 60 / 60 / 24 = 248 days
case class Centis(centis: Int) extends AnyVal with Ordered[Centis] {

  def roundTenths: Int =
    if (centis > 0) (centis + 5) / 10 else (centis - 4) / 10
  def roundSeconds: Int = math.round(centis * 0.01f)

  def toSeconds: BigDecimal = java.math.BigDecimal.valueOf(centis, 2)
  def millis: Long = centis * 10l
  def toDuration = FiniteDuration(millis, MILLISECONDS)

  def +(other: Centis) = Centis(centis + other.centis)
  def -(other: Centis) = Centis(centis - other.centis)
  def *(scalar: Int) = Centis(scalar * centis)
  def /(div: Int) = Centis(centis / div)
  def unary_- = Centis(-centis)

  def avg(other: Centis) = Centis((centis + other.centis) >> 1)

  def compare(other: Centis) = centis - other.centis

  def atMost(o: Centis) = if (centis > o.centis) o else this
  def atLeast(o: Centis) = if (centis < o.centis) o else this

  def nonNeg = if (centis >= 0) this else Centis(0)
}

object Centis {
  implicit val zeroInstance = Zero.instance(Centis(0))
  implicit object CentisMonoid extends Monoid[Centis] {
    def append(c1: Centis, c2: => Centis) = c1 + c2
    val zero = Centis(0)
  }

  def apply(value: Long): Centis = Centis {
    if (value > Int.MaxValue) {
      // lila.log("common").error(s"Truncating Centis! $value")
      Int.MaxValue
    } else if (value < Int.MinValue) {
      // lila.log("common").error(s"Truncating Centis! $value")
      Int.MinValue
    } else value.toInt
  }

  def apply(d: FiniteDuration): Centis = Centis {
    if (d.unit eq MILLISECONDS) d.length / 10
    else d.toMillis / 10
  }

  def ofSeconds(s: Int) = Centis(100 * s)
  def ofMillis(i: Int) = Centis((if (i > 0) i + 5 else i - 4) / 10)
  def ofMillis(l: Long) = Centis((if (l > 0) l + 5 else l - 4) / 10)
}
