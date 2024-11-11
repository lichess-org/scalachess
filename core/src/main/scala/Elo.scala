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
    games.nonEmpty.option:
      val averageOpponentRating = games.map(_.opponentRating).sum / games.size
      val percentageScore = BigDecimal(games.map(_.points.value).sum / games.size).setScale(2, BigDecimal.RoundingMode.HALF_UP)
      averageOpponentRating + performanceRatingTableFIDE.getOrElse(percentageScore, 0)

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


  // 1.4.9 FIDE table
  val performanceRatingTableFIDE: Map[BigDecimal, Int] = Map(
    BigDecimal("0.00") -> -800,
    BigDecimal("0.01") -> -677,
    BigDecimal("0.02") -> -589,
    BigDecimal("0.03") -> -538,
    BigDecimal("0.04") -> -501,
    BigDecimal("0.05") -> -470,
    BigDecimal("0.06") -> -444,
    BigDecimal("0.07") -> -422,
    BigDecimal("0.08") -> -401,
    BigDecimal("0.09") -> -383,
    BigDecimal("0.10") -> -366,
    BigDecimal("0.11") -> -351,
    BigDecimal("0.12") -> -336,
    BigDecimal("0.13") -> -322,
    BigDecimal("0.14") -> -309,
    BigDecimal("0.15") -> -296,
    BigDecimal("0.16") -> -284,
    BigDecimal("0.17") -> -273,
    BigDecimal("0.18") -> -262,
    BigDecimal("0.19") -> -251,
    BigDecimal("0.20") -> -240,
    BigDecimal("0.21") -> -230,
    BigDecimal("0.22") -> -220,
    BigDecimal("0.23") -> -211,
    BigDecimal("0.24") -> -202,
    BigDecimal("0.25") -> -193,
    BigDecimal("0.26") -> -184,
    BigDecimal("0.27") -> -175,
    BigDecimal("0.28") -> -166,
    BigDecimal("0.29") -> -158,
    BigDecimal("0.30") -> -149,
    BigDecimal("0.31") -> -141,
    BigDecimal("0.32") -> -133,
    BigDecimal("0.33") -> -125,
    BigDecimal("0.34") -> -117,
    BigDecimal("0.35") -> -110,
    BigDecimal("0.36") -> -102,
    BigDecimal("0.37") -> -95,
    BigDecimal("0.38") -> -87,
    BigDecimal("0.39") -> -80,
    BigDecimal("0.40") -> -72,
    BigDecimal("0.41") -> -65,
    BigDecimal("0.42") -> -57,
    BigDecimal("0.43") -> -50,
    BigDecimal("0.44") -> -43,
    BigDecimal("0.45") -> -36,
    BigDecimal("0.46") -> -29,
    BigDecimal("0.47") -> -21,
    BigDecimal("0.48") -> -14,
    BigDecimal("0.49") -> -7,
    BigDecimal("0.50") -> 0,
    BigDecimal("0.51") -> 7,
    BigDecimal("0.52") -> 14,
    BigDecimal("0.53") -> 21,
    BigDecimal("0.54") -> 29,
    BigDecimal("0.55") -> 36,
    BigDecimal("0.56") -> 43,
    BigDecimal("0.57") -> 50,
    BigDecimal("0.58") -> 57,
    BigDecimal("0.59") -> 65,
    BigDecimal("0.60") -> 72,
    BigDecimal("0.61") -> 80,
    BigDecimal("0.62") -> 87,
    BigDecimal("0.63") -> 95,
    BigDecimal("0.64") -> 102,
    BigDecimal("0.65") -> 110,
    BigDecimal("0.66") -> 117,
    BigDecimal("0.67") -> 125,
    BigDecimal("0.68") -> 133,
    BigDecimal("0.69") -> 141,
    BigDecimal("0.70") -> 149,
    BigDecimal("0.71") -> 158,
    BigDecimal("0.72") -> 166,
    BigDecimal("0.73") -> 175,
    BigDecimal("0.74") -> 184,
    BigDecimal("0.75") -> 193,
    BigDecimal("0.76") -> 202,
    BigDecimal("0.77") -> 211,
    BigDecimal("0.78") -> 220,
    BigDecimal("0.79") -> 230,
    BigDecimal("0.80") -> 240,
    BigDecimal("0.81") -> 251,
    BigDecimal("0.82") -> 262,
    BigDecimal("0.83") -> 273,
    BigDecimal("0.84") -> 284,
    BigDecimal("0.85") -> 296,
    BigDecimal("0.86") -> 309,
    BigDecimal("0.87") -> 322,
    BigDecimal("0.88") -> 336,
    BigDecimal("0.89") -> 351,
    BigDecimal("0.90") -> 366,
    BigDecimal("0.91") -> 383,
    BigDecimal("0.92") -> 401,
    BigDecimal("0.93") -> 422,
    BigDecimal("0.94") -> 444,
    BigDecimal("0.95") -> 470,
    BigDecimal("0.96") -> 501,
    BigDecimal("0.97") -> 538,
    BigDecimal("0.98") -> 589,
    BigDecimal("0.99") -> 677,
    BigDecimal("1.00") -> 800
  )


