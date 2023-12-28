package chess
package eval

// How likely one is to win a position, based on subjective Stockfish centipawns
opaque type WinPercent = Double
object WinPercent extends OpaqueDouble[WinPercent]:

  def apply(score: Score): WinPercent = score match
    case Score.Cp(cp)     => fromCp(cp)
    case Score.Mate(mate) => fromMate(mate)

  def fromMate(mate: Mate): WinPercent =
    fromCp(Cp.ceilingWithSignum(mate.signum))

  def fromCp(cp: Cp): WinPercent = WinPercent:
    50 + 50 * winningChances(cp.ceiled)

  inline def fromPercent(int: Int) = WinPercent(int.toDouble)

  val initial = fromCp(Cp.initial)

  // [-1, +1]
  def winningChances(cp: Cp): Double =
    val MULTIPLIER = -0.00368208 // https://github.com/lichess-org/lila/pull/11148
    val unbounded  = 2 / (1 + Math.exp(MULTIPLIER * cp.value)) - 1
    math.max(-1, math.min(1, unbounded))
