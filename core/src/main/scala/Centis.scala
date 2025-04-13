package chess

import alleycats.Zero
import cats.kernel.Monoid
import scalalib.model.Seconds

import scala.concurrent.duration.*

// maximum centis = Int.MaxValue / 100 / 60 / 60 / 24 = 248 days
opaque type Centis = Int
object Centis extends RichOpaqueInt[Centis]:

  extension (centis: Centis)

    inline def centis: Int = centis

    inline def *(inline o: Int): Centis = centis * o

    def roundTenths: Int      = (if centis > 0 then centis + 5 else centis - 4) / 10
    def roundSeconds: Seconds = Seconds(Math.round(centis * 0.01f))

    inline def toSeconds: BigDecimal = java.math.BigDecimal.valueOf(centis, 2)
    inline def millis: Long          = centis * 10L
    def toDuration: FiniteDuration   = FiniteDuration(millis, MILLISECONDS)

    def *~(scalar: Float): Centis   = ofFloat(scalar * centis)
    def /(div: Int): Option[Centis] = (div != 0).option(centis / div)

    def avg(other: Centis): Centis = (centis + other.value) >> 1

    inline def nonNeg: Centis = Math.max(centis, 0)

  end extension

  given Zero[Centis] = Zero(0)

  given Monoid[Centis] with
    def combine(c1: Centis, c2: Centis) = c1 + c2
    val empty                           = 0

  def ofLong(l: Long): Centis =
    try Math.toIntExact(l)
    catch
      case _: ArithmeticException =>
        if l > 0 then Integer.MAX_VALUE
        else Integer.MIN_VALUE

  def apply(d: FiniteDuration): Centis =
    ofMillis:
      if d.unit eq MILLISECONDS then d.length
      else d.toMillis

  inline def ofFloat(f: Float): Centis   = Math.round(f)
  inline def ofDouble(d: Double): Centis = ofLong(Math.round(d))

  inline def ofSeconds(s: Int): Centis = 100 * s
  inline def ofMillis(l: Long): Centis = ofLong(if l > 0 then l + 5 else l - 4) / 10
