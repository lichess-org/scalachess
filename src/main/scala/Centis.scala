package chess

import scala.concurrent.duration.*

import cats.Monoid
import alleycats.Zero

// maximum centis = Int.MaxValue / 100 / 60 / 60 / 24 = 248 days
opaque type Centis = Int
object Centis extends OpaqueInt[Centis]:

  extension (centis: Centis)

    inline def centis: Int = centis

    inline def *(inline o: Int): Centis = centis * o

    def roundTenths: Int  = if (centis > 0) (centis + 5) / 10 else (centis - 4) / 10
    def roundSeconds: Int = Math.round(centis * 0.01f)

    // def toSeconds: BigDecimal = java.math.BigDecimal.valueOf(centis, 2)
    inline def millis: Long  = centis * 10L
    def toDuration: Duration = FiniteDuration(millis, MILLISECONDS)

    def *~(scalar: Float): Centis = ofFloat(scalar * centis)
    // def *~(scalar: Double): Centis  = Centis(scalar * centis)
    def /(div: Int): Option[Centis] = div != 0 option (centis / div)

    def avg(other: Centis): Centis = (centis + other.value) >> 1

    inline def nonNeg: Centis = Math.max(centis, 0)

  end extension

  given Zero[Centis] = Zero(0)

  given Monoid[Centis] with
    def combine(c1: Centis, c2: Centis) = c1 + c2
    val empty                           = 0

  // extension (scalar: Int) def *(o: Centis) = o * scalar
  // extension (scalar: Float) def *~(o: Centis) = o *~ scalar
  // extension (scalar: Double) def *~(o: Centis) = o *~ scalar

  def ofLong(l: Long): Centis =
    if (l.toInt == l) l.toInt
    else if (l > 0) Integer.MAX_VALUE
    else Integer.MIN_VALUE

  def apply(d: FiniteDuration): Centis =
    ofMillis {
      if (d.unit eq MILLISECONDS) d.length
      else d.toMillis
    }

  inline def ofFloat(f: Float): Centis = Math.round(f)
  // def ofDouble(d: Double): Centis = Math.round(d)

  inline def ofSeconds(s: Int): Centis = 100 * s
  // def ofMillis(i: Int): Centis  = (if (i > 0) i + 5 else i - 4) / 10
  inline def ofMillis(l: Long): Centis = ofLong(if (l > 0) l + 5 else l - 4) / 10
