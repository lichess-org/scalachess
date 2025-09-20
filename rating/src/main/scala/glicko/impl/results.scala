package chess.rating.glicko
package impl

private[glicko] trait Result:

  def getAdvantage(advantage: ColorAdvantage, p: Rating): ColorAdvantage

  def getScore(player: Rating): Double

  def getOpponent(player: Rating): Rating

  def participated(player: Rating): Boolean

  def players: List[Rating]

// score from 0 (opponent wins) to 1 (player wins)
final private[glicko] class FloatingResult(player: Rating, opponent: Rating, score: Float) extends Result:

  def getAdvantage(advantage: ColorAdvantage, p: Rating): ColorAdvantage = ColorAdvantage.zero

  def getScore(p: Rating) = if p == player then score else 1 - score

  def getOpponent(p: Rating) = if p == player then opponent else player

  def participated(p: Rating) = p == player || p == opponent

  def players = List(player, opponent)

final private[glicko] class GameResult(first: Rating, second: Rating, outcome: chess.Outcome) extends Result:
  private val POINTS_FOR_WIN = 1.0d
  private val POINTS_FOR_LOSS = 0.0d
  private val POINTS_FOR_DRAW = 0.5d

  def players = List(first, second)

  def participated(player: Rating) = player == first || player == second

  def getAdvantage(advantage: ColorAdvantage, player: Rating): ColorAdvantage =
    if player == first then advantage.half else advantage.negate.half

  /** Returns the "score" for a match.
    *
    * @param player
    * @return
    *   1 for a win, 0.5 for a draw and 0 for a loss
    * @throws IllegalArgumentException
    */
  def getScore(player: Rating): Double = outcome.winner match
    case Some(chess.Color.White) => if player == first then POINTS_FOR_WIN else POINTS_FOR_LOSS
    case Some(chess.Color.Black) => if player == first then POINTS_FOR_LOSS else POINTS_FOR_WIN
    case _ =>
      if participated(player) then POINTS_FOR_DRAW
      else throw new IllegalArgumentException("Player did not participate in match");

  def getOpponent(player: Rating) =
    if first == player then second
    else if second == player then first
    else throw new IllegalArgumentException("Player did not participate in match");

  override def toString = s"$first vs $second = $outcome"

private[glicko] trait RatingPeriodResults[R <: Result]():
  val results: List[R]
  def getResults(player: Rating): List[R] = results.filter(_.participated(player))
  def getParticipants: Set[Rating] = results.flatMap(_.players).toSet

final private[glicko] class GameRatingPeriodResults(val results: List[GameResult])
    extends RatingPeriodResults[GameResult]

final private[glicko] class FloatingRatingPeriodResults(val results: List[FloatingResult])
    extends RatingPeriodResults[FloatingResult]
