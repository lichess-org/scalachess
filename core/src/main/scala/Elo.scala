package chess

import cats.syntax.all.*

opaque type Elo = Int

opaque type KFactor = Int
object KFactor extends OpaqueInt[KFactor]:
  val default = KFactor(40)

/*
 * https://handbook.fide.com/chapter/B022022
 * https://ratings.fide.com/calc.phtml
 * */
object Elo extends RelaxedOpaqueInt[Elo]:

  def computeRatingDiff(player: Player, games: Seq[Game]): Int =
    computeNewRating(player, games) - player.rating

  def computeNewRating(player: Player, games: Seq[Game]): Elo =
    val expectedScore = games.foldMap: game =>
      val prd = playersRatingDiff(player.rating, game.opponentRating)
      getExpectedScore(prd)
    val achievedScore = games.foldMap(_.points.value)
    val ratingDiff =
      Math.round(player.kFactor * (achievedScore - expectedScore))
    player.rating + ratingDiff

  /* 8.3.1
   * For each game played against a rated player, determine the difference in rating between the player and their opponent.
   * A difference in rating of more than 400 points shall be counted for rating purposes as though it were a difference of 400 points.  In any tournament, a player may benefit from only one upgrade under this rule, for the game in which the rating difference is greatest. */
  def playersRatingDiff(a: Elo, b: Elo): Int =
    Math.min(400, Math.max(-400, b - a))

  def getExpectedScore(ratingDiff: Int): Float =
    val absRatingDiff = ratingDiff.abs
    val expectedScore = conversionTableFIDE.getOrElse(absRatingDiff, 0.92f)
    if ratingDiff <= 0 then expectedScore else 1.0f - expectedScore

  def computePerformanceRating(games: Seq[Game]): Option[Elo] =
    val winBonus = 400
    games.nonEmpty.option:
      val ratings = games.map(_.opponentRating).sum
      val points = games.foldMap:
        _.points.match
          case Outcome.Points.Zero => -1
          case Outcome.Points.Half => 0
          case Outcome.Points.One  => 1
      (ratings + points * winBonus) / games.size

  final class Player(val rating: Elo, val kFactor: KFactor)

  final class Game(val points: Outcome.Points, val opponentRating: Elo)

  // 8.1.2 FIDE table
  val conversionTableFIDE: Map[Int, Float] = List(
    3   -> 0.50f,
    10  -> 0.51f,
    17  -> 0.52f,
    25  -> 0.53f,
    32  -> 0.54f,
    39  -> 0.55f,
    46  -> 0.56f,
    53  -> 0.57f,
    61  -> 0.58f,
    68  -> 0.59f,
    76  -> 0.60f,
    83  -> 0.61f,
    91  -> 0.62f,
    98  -> 0.63f,
    106 -> 0.64f,
    113 -> 0.65f,
    121 -> 0.66f,
    129 -> 0.67f,
    137 -> 0.68f,
    145 -> 0.69f,
    153 -> 0.70f,
    162 -> 0.71f,
    170 -> 0.72f,
    179 -> 0.73f,
    188 -> 0.74f,
    197 -> 0.75f,
    206 -> 0.76f,
    215 -> 0.77f,
    225 -> 0.78f,
    235 -> 0.79f,
    245 -> 0.80f,
    256 -> 0.81f,
    267 -> 0.82f,
    278 -> 0.83f,
    290 -> 0.84f,
    302 -> 0.85f,
    315 -> 0.86f,
    328 -> 0.87f,
    344 -> 0.88f,
    357 -> 0.89f,
    374 -> 0.90f,
    391 -> 0.91f
  ).foldLeft(0 -> Map.empty[Int, Float]):
    case ((low, table), (up, value)) =>
      val newTable = table ++
        (low to up).view.map(_ -> value).toMap
      (up + 1) -> newTable
  ._2
  // the hardcoded List above is not really necessary,
  // but it mirrors the reference table on
  // https://handbook.fide.com/chapter/B022022 8.1.2
