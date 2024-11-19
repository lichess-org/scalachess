package chess
package glicko

import java.time.Instant

case class Player(
    rating: Double,
    ratingDeviation: Double,
    volatility: Double,
    numberOfResults: Int,
    lastRatingPeriodEnd: Option[Instant] = None
)

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
  def computeGame(game: Game, skipDeviationIncrease: Boolean = false): ByColor[Player]

  /** This is the formula defined in step 6. It is also used for players who have not competed during the rating period.
    */
  // def previewDeviation(player: Player, ratingPeriodEndDate: Instant, reverse: Boolean): Double

final class GlickoCalculator(config: Config) extends GlickoCalculatorApi:

  private val calculator = chess.glicko.impl.RatingCalculator(config.tau, config.ratingPeriodsPerDay)

  // Simpler use case: a single game
  def computeGame(game: Game, skipDeviationIncrease: Boolean = false): ByColor[Player] =
    val ratings       = game.players.map(conversions.toRating)
    val gameResult    = conversions.toGameResult(ratings, game.outcome)
    val periodResults = impl.GameRatingPeriodResults(List(gameResult))
    calculator.updateRatings(periodResults, skipDeviationIncrease)
    ratings.map(conversions.toPlayer)

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
      ratingDeviation = player.ratingDeviation,
      volatility = player.volatility,
      numberOfResults = player.numberOfResults,
      lastRatingPeriodEnd = player.lastRatingPeriodEnd
    )

    def toPlayer(rating: Rating) = Player(
      rating = rating.rating,
      ratingDeviation = rating.ratingDeviation,
      volatility = rating.volatility,
      numberOfResults = rating.numberOfResults,
      lastRatingPeriodEnd = rating.lastRatingPeriodEnd
    )
