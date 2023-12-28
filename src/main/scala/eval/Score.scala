package chess
package eval

opaque type Cp = Int
object Cp extends OpaqueInt[Cp]:
  val CEILING: Cp                               = Cp(1000)
  val initial: Cp                               = Cp(15)
  inline def ceilingWithSignum(signum: Int): Cp = CEILING.invertIf(signum < 0)

  extension (cp: Cp)
    inline def centipawns = cp.value

    inline def pawns: Float      = cp.value / 100f
    inline def showPawns: String = "%.2f" format pawns

    def ceiled: Cp =
      if cp > Cp.CEILING then Cp.CEILING
      else if cp < -Cp.CEILING then -Cp.CEILING
      else cp

    inline def invert: Cp                  = -cp
    inline def invertIf(cond: Boolean): Cp = if cond then invert else cp

    def signum: Int = Math.signum(cp.value.toFloat).toInt

opaque type Mate = Int
object Mate extends OpaqueInt[Mate]:
  extension (mate: Mate)
    inline def moves: Int = mate.value

    inline def invert: Mate                  = Mate(-moves)
    inline def invertIf(cond: Boolean): Mate = if cond then invert else mate

    inline def signum: Int = if positive then 1 else -1

    inline def positive = mate.value > 0
    inline def negative = mate.value < 0

enum Score:
  case Cp(c: eval.Cp)
  case Mate(m: eval.Mate)

  inline def fold[A](w: eval.Cp => A, b: eval.Mate => A): A = this match
    case Score.Cp(cp)     => w(cp)
    case Score.Mate(mate) => b(mate)

  inline def cp: Option[eval.Cp]     = fold(Some(_), _ => None)
  inline def mate: Option[eval.Mate] = fold(_ => None, Some(_))

  inline def isCheckmate = mate.exists(_.value == 0)
  inline def mateFound   = mate.isDefined

  inline def invert: Score                  = fold(c => Score.Cp(c.invert), m => Score.Mate(m.invert))
  inline def invertIf(cond: Boolean): Score = if cond then invert else this

object Score:
  def cp(cp: Int): Score     = Cp(eval.Cp(cp))
  def mate(mate: Int): Score = Mate(eval.Mate(mate))
