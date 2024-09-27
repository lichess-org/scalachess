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

  def computeNewRating(player: Player, games: Seq[Game]): Elo =
    val expectedScore = games.foldMap: game =>
      val prd = playersRatingDiff(player.rating, game.opponentRating)
      getExpectedScore(prd)
    val achievedScore = games.foldMap(_.points.value)
    val ratingDiff =
      Math.round(player.kFactor * (achievedScore - expectedScore)).toInt
    player.rating + ratingDiff

  def computeRatingDiff(player: Player, games: Seq[Game]): Int =
    computeNewRating(player, games) - player.rating

  def getExpectedScore(ratingDiff: Int): Double =
    val absRatingDiff = ratingDiff.abs
    val expectedScore = conversionTableFIDE.collectFirst {
      case (range, pd) if range.contains(absRatingDiff) => pd
    }.getOrElse {
      throw new IllegalArgumentException(s"Invalid rating difference: $ratingDiff")
    }

    if ratingDiff <= 0 then expectedScore else 1.0 - expectedScore

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

  // 8.1.2 FIDE table
  val conversionTableFIDE: Map[Range.Inclusive, Double] = Map(
    (0 to 3)              -> 0.50,
    (4 to 10)             -> 0.51,
    (11 to 17)            -> 0.52,
    (18 to 25)            -> 0.53,
    (26 to 32)            -> 0.54,
    (33 to 39)            -> 0.55,
    (40 to 46)            -> 0.56,
    (47 to 53)            -> 0.57,
    (54 to 61)            -> 0.58,
    (62 to 68)            -> 0.59,
    (69 to 76)            -> 0.60,
    (77 to 83)            -> 0.61,
    (84 to 91)            -> 0.62,
    (92 to 98)            -> 0.63,
    (99 to 106)           -> 0.64,
    (107 to 113)          -> 0.65,
    (114 to 121)          -> 0.66,
    (122 to 129)          -> 0.67,
    (130 to 137)          -> 0.68,
    (138 to 145)          -> 0.69,
    (146 to 153)          -> 0.70,
    (154 to 162)          -> 0.71,
    (163 to 170)          -> 0.72,
    (171 to 179)          -> 0.73,
    (180 to 188)          -> 0.74,
    (189 to 197)          -> 0.75,
    (198 to 206)          -> 0.76,
    (207 to 215)          -> 0.77,
    (216 to 225)          -> 0.78,
    (226 to 235)          -> 0.79,
    (236 to 245)          -> 0.80,
    (246 to 256)          -> 0.81,
    (257 to 267)          -> 0.82,
    (268 to 278)          -> 0.83,
    (279 to 290)          -> 0.84,
    (291 to 302)          -> 0.85,
    (303 to 315)          -> 0.86,
    (316 to 328)          -> 0.87,
    (329 to 344)          -> 0.88,
    (345 to 357)          -> 0.89,
    (358 to 374)          -> 0.90,
    (375 to 391)          -> 0.91,
    (392 to Int.MaxValue) -> 0.92
  )
