package chess
package glicko

import java.time.Instant
import scala.util.Try

case class Glicko(
    rating: Double,
    deviation: Double,
    volatility: Double
):
  def intRating: IntRating = IntRating(rating.toInt)
  def intDeviation         = deviation.toInt
  def provisional          = RatingProvisional(deviation >= Glicko.provisionalDeviation)
  def established          = provisional.no
  def establishedIntRating = Option.when(established)(intRating)
  def clueless             = deviation >= Glicko.cluelessDeviation
  def display              = s"$intRating${if provisional.yes then "?" else ""}"
  def average(other: Glicko, weight: Float = 0.5f): Glicko =
    if weight >= 1 then other
    else if weight <= 0 then this
    else
      Glicko(
        rating = rating * (1 - weight) + other.rating * weight,
        deviation = deviation * (1 - weight) + other.deviation * weight,
        volatility = volatility * (1 - weight) + other.volatility * weight
      )
  override def toString = f"$intRating/$intDeviation/${volatility}%.3f"

object Glicko:
  val provisionalDeviation = 110
  val cluelessDeviation    = 230

case class Player(
    glicko: Glicko,
    numberOfResults: Int,
    lastRatingPeriodEnd: Option[Instant] = None
):
  export glicko.*

case class Game(players: ByColor[Player], outcome: Outcome)

case class Config(
    tau: impl.Tau = impl.Tau.default,
    ratingPeriodsPerDay: impl.RatingPeriodsPerDay = impl.RatingPeriodsPerDay.default
)

/* Purely functional interface hiding the mutable implementation */
trait GlickoCalculatorApi:

  /** Apply rating calculations and return updated players.
    * Note that players who did not compete during the rating period will have see their deviation increase.
    * This requires players to have some sort of unique identifier.
    */
  // def computeGames(
  //     games: List[Game],
  //     skipDeviationIncrease: Boolean = false
  // ): List[Player]

  // Simpler use case: a single game
  def computeGame(game: Game, skipDeviationIncrease: Boolean = false): Try[ByColor[Player]]

  /** This is the formula defined in step 6. It is also used for players who have not competed during the rating period.
    */
  def previewDeviation(player: Player, ratingPeriodEndDate: Instant, reverse: Boolean): Double

final class GlickoCalculator(config: Config) extends GlickoCalculatorApi:

  private val calculator = chess.glicko.impl.RatingCalculator(config.tau, config.ratingPeriodsPerDay)

  // Simpler use case: a single game
  def computeGame(game: Game, skipDeviationIncrease: Boolean = false): Try[ByColor[Player]] =
    val ratings       = game.players.map(conversions.toRating)
    val gameResult    = conversions.toGameResult(ratings, game.outcome)
    val periodResults = impl.GameRatingPeriodResults(List(gameResult))
    Try:
      calculator.updateRatings(periodResults, skipDeviationIncrease)
      ratings.map(conversions.toPlayer)

  def previewDeviation(player: Player, ratingPeriodEndDate: Instant, reverse: Boolean): Double =
    calculator.previewDeviation(conversions.toRating(player), ratingPeriodEndDate, reverse)

  private object conversions:

    import impl.*

    def toGameResult(ratings: ByColor[Rating], outcome: Outcome): GameResult =
      outcome.winner match
        case None        => GameResult(ratings.white, ratings.black, true)
        case Some(White) => GameResult(ratings.white, ratings.black, false)
        case Some(Black) => GameResult(ratings.black, ratings.white, false)

    def gamesToPeriodResults(games: List[Game]) = GameRatingPeriodResults:
      games.map: game =>
        toGameResult(game.players.map(toRating), game.outcome)

    def toRating(player: Player) = impl.Rating(
      rating = player.rating,
      ratingDeviation = player.deviation,
      volatility = player.volatility,
      numberOfResults = player.numberOfResults,
      lastRatingPeriodEnd = player.lastRatingPeriodEnd
    )

    def toPlayer(rating: Rating) = Player(
      glicko = Glicko(
        rating = rating.rating,
        deviation = rating.ratingDeviation,
        volatility = rating.volatility
      ),
      numberOfResults = rating.numberOfResults,
      lastRatingPeriodEnd = rating.lastRatingPeriodEnd
    )
