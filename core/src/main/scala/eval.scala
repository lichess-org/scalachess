package chess
package eval

import scalalib.model.Percent

enum Score:
  case Cp(c: Eval.Cp)
  case Mate(m: Eval.Mate) // Mate(0) is a loss
  case MateGiven // Win

  inline def fold[A](cp: Eval.Cp => A, mate: Eval.Mate => A, mateGiven: => A): A = this match
    case Cp(c) => cp(c)
    case Mate(m) => mate(m)
    case MateGiven => mateGiven

  inline def cp: Option[Eval.Cp] = fold(Some(_), _ => None, None)
  inline def mate: Option[Eval.Mate] = fold(_ => None, Some(_), None)

  inline def isMateFound = fold(_ => false, _ => true, true)
  inline def isGameOver = fold(_ => false, _.value == 0, true)

  def invert: Score = this match
    case Cp(c) => Cp(c.invert)
    case Score.mated => MateGiven
    case Mate(m) => Mate(Eval.Mate(-m.value))
    case MateGiven => Score.mated
  inline def invertIf(cond: Boolean): Score = if cond then invert else this

object Score:
  val mated = Mate(Eval.Mate(0))
  def cp(cp: Int): Score = Cp(Eval.Cp(cp))
  def mate(mate: Int): Score = Mate(Eval.Mate(mate))

opaque type WhiteScore = Score
object WhiteScore:
  def apply(score: Score, turn: Color): WhiteScore = score.invertIf(turn.black)
  inline def fromWhite(score: Score): WhiteScore = score

  val initial: WhiteScore = Score.cp(15)

  extension (score: WhiteScore)
    def pov(turn: Color): Score = score.invertIf(turn.black)
    inline def white: Score = score

    inline def isMateFound: Boolean = score.isMateFound
    inline def isGameOver: Boolean = score.isGameOver

    // Mate value from the point of view of white, where Some(0) means the
    // player to move is mated (the only possible meaning in standard chess,
    // but not in variants).
    def exportMate(turn: Color): Option[Int] = score.pov(turn) match
      case Score.mated => Some(0)
      case Score.MateGiven => None
      case _ => score.mate.map(_.moves)

object Eval:
  opaque type Cp = Int
  object Cp extends OpaqueInt[Cp]:
    val CEILING = Cp(1000)
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

      inline def positive = mate.value > 0
      inline def negative = mate.value < 0

// How likely one is to win a position, based on subjective Stockfish centipawns
opaque type WinPercent = Double
object WinPercent extends OpaqueDouble[WinPercent]:

  // given lila.db.NoDbHandler[WinPercent] with {}
  given Percent[WinPercent] = Percent.of(WinPercent)

  extension (a: WinPercent) def toInt = Percent.toInt(a)

  def fromScore(score: Score): WinPercent =
    score.fold(fromCentiPawns, fromMate, fromCentiPawns(Eval.Cp.CEILING))

  def fromMate(mate: Eval.Mate) = fromCentiPawns(Eval.Cp.CEILING.invertIf(!mate.positive))

  // [0, 100]
  def fromCentiPawns(cp: Eval.Cp) = WinPercent:
    50 + 50 * winningChances(cp.ceiled)

  inline def fromPercent(int: Int) = WinPercent(int.toDouble)

  // [-1, +1]
  def winningChances(cp: Eval.Cp) = {
    val MULTIPLIER = -0.00368208 // https://github.com/lichess-org/lila/pull/11148
    2 / (1 + Math.exp(MULTIPLIER * cp.value)) - 1
  }.atLeast(-1).atMost(+1)
