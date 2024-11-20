package chess.rating
package glicko

import chess.{Black, ByColor, Outcome, White}

import java.time.Instant
import scala.util.Try

/* Purely functional interface hiding the mutable implementation */
final class GlickoCalculator(
    tau: Tau = Tau.default,
    ratingPeriodsPerDay: RatingPeriodsPerDay = RatingPeriodsPerDay.default
):

  private val calculator = new impl.RatingCalculator(tau, ratingPeriodsPerDay)

  // Simpler use case: a single game
  def computeGame(game: Game, skipDeviationIncrease: Boolean = false): Try[ByColor[Player]] =
    val ratings       = game.players.map(conversions.toRating)
    val gameResult    = conversions.toGameResult(ratings, game.outcome)
    val periodResults = impl.GameRatingPeriodResults(List(gameResult))
    Try:
      calculator.updateRatings(periodResults, skipDeviationIncrease)
      ratings.map(conversions.toPlayer)

  /** This is the formula defined in step 6. It is also used for players who have not competed during the rating period. */
  def previewDeviation(player: Player, ratingPeriodEndDate: Instant, reverse: Boolean): Double =
    calculator.previewDeviation(conversions.toRating(player), ratingPeriodEndDate, reverse)

  /** Apply rating calculations and return updated players.
    * Note that players who did not compete during the rating period will have see their deviation increase.
    * This requires players to have some sort of unique identifier.
    */
  // def computeGames( games: List[Game], skipDeviationIncrease: Boolean = false): List[Player]

  private object conversions:

    import impl.*

    def toGameResult(ratings: ByColor[Rating], outcome: Outcome): GameResult =
      outcome.winner match
        case None        => GameResult(ratings.white, ratings.black, true)
        case Some(White) => GameResult(ratings.white, ratings.black, false)
        case Some(Black) => GameResult(ratings.black, ratings.white, false)

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
