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
object Elo extends OpaqueInt[Elo]:

  final class Player(val rating: Elo, val kFactor: KFactor)
  final class Game(val points: Outcome.Points, val opponentRating: Elo)

  /* 8.3.1
   * For each game played against a rated player, determine the difference in rating between the player and their opponent.
   * A difference in rating of more than 400 points shall be counted for rating purposes as though it were a difference of 400 points.  In any tournament, a player may benefit from only one upgrade under this rule, for the game in which the rating difference is greatest. */
  def playersRatingDiff(a: Elo, b: Elo): Int =
    Math.min(400, Math.max(-400, b - a))

  // https://en.wikipedia.org/wiki/Elo_rating_system#Mathematical_details
  def computeNewRating(player: Player, games: Seq[Game]): Elo =
    val expectedScore = games.foldMap: game =>
      val prd = playersRatingDiff(player.rating, game.opponentRating)
      1 / (1 + Math.pow(10, prd / 400d))
    val achievedScore = games.foldMap(_.points.value)
    val ratingDiff =
      Math.round(player.kFactor * (achievedScore - expectedScore)).toInt
    player.rating + ratingDiff

  def computeRatingDiff(player: Player, games: Seq[Game]): Int =
    computeNewRating(player, games) - player.rating

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
