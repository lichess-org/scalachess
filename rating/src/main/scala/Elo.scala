package chess.rating

import cats.syntax.all.*
import scalalib.extensions.*
import scalalib.newtypes.*

opaque type Elo = Int

opaque type KFactor = Int
object KFactor extends OpaqueInt[KFactor]:
  val default = KFactor(40)

/*
 * https://handbook.fide.com/chapter/B022022
 * https://ratings.fide.com/calc.phtml
 * */
object Elo extends RichOpaqueInt[Elo]:

  def computeRatingDiff(player: Player, games: Seq[Game]): IntRatingDiff =
    IntRatingDiff(computeNewRating(player, games) - player.rating)

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
   * A difference in rating of more than 400 points shall be counted for rating purposes as though it were a difference of 400 points.  In any tournament, a player may benefit from only one upgrade under this rule, for the game in which the rating difference is greatest.
   * For players rated 2650 and above, the difference between ratings shall be used in all cases.”
   */
  def playersRatingDiff(player: Elo, opponent: Elo): Int =
    val ratingDiff = opponent - player
    if player >= 2650 then ratingDiff else ratingDiff.atLeast(-400).atMost(400)

  def getExpectedScore(ratingDiff: Int): Float =
    val absRatingDiff = ratingDiff.abs
    val expectedScore = conversionTableFIDE.getOrElse(absRatingDiff, 0.99f)
    if ratingDiff <= 0 then expectedScore else 1.0f - expectedScore

  def computePerformanceRating(games: Seq[Game]): Option[Elo] =
    games.nonEmpty.option:
      val averageOpponentRating = games.map(_.opponentRating).sum / games.size
      val percentageScore = Math.round(games.map(_.points.value).sum * 100 / games.size)
      averageOpponentRating + performanceRatingTableFIDE.getOrElse(percentageScore, 0)

  final class Player(val rating: Elo, val kFactor: KFactor)

  final class Game(val points: chess.Outcome.Points, val opponentRating: Elo)
  // 8.1.2 FIDE table
  // We use the full table for perfect tournament performance rating  (PTP) calculations (no +- 400 limit)
  val conversionTableFIDE: Map[Int, Float] = List(
    3 -> 0.50f,
    10 -> 0.51f,
    17 -> 0.52f,
    25 -> 0.53f,
    32 -> 0.54f,
    39 -> 0.55f,
    46 -> 0.56f,
    53 -> 0.57f,
    61 -> 0.58f,
    68 -> 0.59f,
    76 -> 0.60f,
    83 -> 0.61f,
    91 -> 0.62f,
    98 -> 0.63f,
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
    391 -> 0.91f,
    411 -> 0.92f,
    432 -> 0.93f,
    456 -> 0.94f,
    484 -> 0.95f,
    517 -> 0.96f,
    559 -> 0.97f,
    619 -> 0.98f,
    735 -> 0.99f
  ).foldLeft(0 -> Map.empty[Int, Float]):
    case ((low, table), (up, value)) =>
      val newTable = table ++
        (low to up).view.map(_ -> value).toMap
      (up + 1) -> newTable
  ._2
  // the hardcoded List above is not really necessary,
  // but it mirrors the reference table on
  // https://handbook.fide.com/chapter/B022022 8.1.2

  // 1.4.9 FIDE table
  val performanceRatingTableFIDE: Map[Int, Int] = Map(
    0 -> -800,
    1 -> -677,
    2 -> -589,
    3 -> -538,
    4 -> -501,
    5 -> -470,
    6 -> -444,
    7 -> -422,
    8 -> -401,
    9 -> -383,
    10 -> -366,
    11 -> -351,
    12 -> -336,
    13 -> -322,
    14 -> -309,
    15 -> -296,
    16 -> -284,
    17 -> -273,
    18 -> -262,
    19 -> -251,
    20 -> -240,
    21 -> -230,
    22 -> -220,
    23 -> -211,
    24 -> -202,
    25 -> -193,
    26 -> -184,
    27 -> -175,
    28 -> -166,
    29 -> -158,
    30 -> -149,
    31 -> -141,
    32 -> -133,
    33 -> -125,
    34 -> -117,
    35 -> -110,
    36 -> -102,
    37 -> -95,
    38 -> -87,
    39 -> -80,
    40 -> -72,
    41 -> -65,
    42 -> -57,
    43 -> -50,
    44 -> -43,
    45 -> -36,
    46 -> -29,
    47 -> -21,
    48 -> -14,
    49 -> -7,
    50 -> 0,
    51 -> 7,
    52 -> 14,
    53 -> 21,
    54 -> 29,
    55 -> 36,
    56 -> 43,
    57 -> 50,
    58 -> 57,
    59 -> 65,
    60 -> 72,
    61 -> 80,
    62 -> 87,
    63 -> 95,
    64 -> 102,
    65 -> 110,
    66 -> 117,
    67 -> 125,
    68 -> 133,
    69 -> 141,
    70 -> 149,
    71 -> 158,
    72 -> 166,
    73 -> 175,
    74 -> 184,
    75 -> 193,
    76 -> 202,
    77 -> 211,
    78 -> 220,
    79 -> 230,
    80 -> 240,
    81 -> 251,
    82 -> 262,
    83 -> 273,
    84 -> 284,
    85 -> 296,
    86 -> 309,
    87 -> 322,
    88 -> 336,
    89 -> 351,
    90 -> 366,
    91 -> 383,
    92 -> 401,
    93 -> 422,
    94 -> 444,
    95 -> 470,
    96 -> 501,
    97 -> 538,
    98 -> 589,
    99 -> 677,
    100 -> 800
  )
