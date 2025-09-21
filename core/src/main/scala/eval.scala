package chess
package eval

import scalalib.model.Percent

enum Score:
  case Cp(c: Eval.Cp)
  case Mate(m: Eval.Mate)

  inline def fold[A](w: Eval.Cp => A, b: Eval.Mate => A): A = this match
    case Cp(cp) => w(cp)
    case Mate(mate) => b(mate)

  inline def cp: Option[Eval.Cp] = fold(Some(_), _ => None)
  inline def mate: Option[Eval.Mate] = fold(_ => None, Some(_))

  inline def isCheckmate = mate.exists(_.value == 0)
  inline def mateFound = mate.isDefined

  inline def invert: Score = fold(c => Cp(c.invert), m => Mate(m.invert))
  inline def invertIf(cond: Boolean): Score = if cond then invert else this

object Score:
  val initial = Cp(Eval.Cp.initial)
  def cp(cp: Int): Score = Cp(Eval.Cp(cp))
  def mate(mate: Int): Score = Mate(Eval.Mate(mate))

object Eval:
  opaque type Cp = Int
  object Cp extends OpaqueInt[Cp]:
    val CEILING = Cp(1000)
    val initial = Cp(15)
    inline def ceilingWithSignum(signum: Int) = CEILING.invertIf(signum < 0)

    extension (cp: Cp)
      inline def centipawns = cp.value

      inline def pawns: Float = cp.value / 100f
      inline def showPawns: String = "%.2f".format(pawns)

      inline def ceiled: Cp =
        if cp.value > Cp.CEILING then Cp.CEILING
        else if cp.value < -Cp.CEILING then -Cp.CEILING
        else cp

      inline def invert: Cp = Cp(-cp.value)
      inline def invertIf(cond: Boolean): Cp = if cond then invert else cp

      def signum: Int = Math.signum(cp.value.toFloat).toInt

  end Cp

  opaque type Mate = Int
  object Mate extends OpaqueInt[Mate]:
    extension (mate: Mate)
      inline def moves: Int = mate.value

      inline def invert: Mate = Mate(-moves)
      inline def invertIf(cond: Boolean): Mate = if cond then invert else mate

      inline def signum: Int = if positive then 1 else -1

      inline def positive = mate.value > 0
      inline def negative = mate.value < 0

// How likely one is to win a position, based on subjective Stockfish centipawns
opaque type WinPercent = Double
object WinPercent extends OpaqueDouble[WinPercent]:

  // given lila.db.NoDbHandler[WinPercent] with {}
  given Percent[WinPercent] = Percent.of(WinPercent)

  extension (a: WinPercent) def toInt = Percent.toInt(a)

  def fromScore(score: Score): WinPercent = score.fold(fromCentiPawns, fromMate)

  def fromMate(mate: Eval.Mate) = fromCentiPawns(Eval.Cp.ceilingWithSignum(mate.signum))

  // [0, 100]
  def fromCentiPawns(cp: Eval.Cp) = WinPercent:
    50 + 50 * winningChances(cp.ceiled)

  inline def fromPercent(int: Int) = WinPercent(int.toDouble)

  // [-1, +1]
  def winningChances(cp: Eval.Cp) = {
    val MULTIPLIER = -0.00368208 // https://github.com/lichess-org/lila/pull/11148
    2 / (1 + Math.exp(MULTIPLIER * cp.value)) - 1
  }.atLeast(-1).atMost(+1)
