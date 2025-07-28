package chess.rating.glicko
package impl

final private[glicko] class Rating(
    var rating: Double,
    var ratingDeviation: Double,
    var volatility: Double,
    var numberOfResults: Int,
    var lastRatingPeriodEnd: Option[java.time.Instant] = None
):

  import RatingCalculator.*

  // the following variables are used to hold values temporarily whilst running calculations
  private[impl] var workingRating: Double = scala.compiletime.uninitialized
  private[impl] var workingRatingDeviation: Double = scala.compiletime.uninitialized
  private[impl] var workingVolatility: Double = scala.compiletime.uninitialized

  /** Return the average skill value of the player scaled down to the scale used by the algorithm's internal
    * workings.
    */
  private[impl] def getGlicko2Rating: Double = convertRatingToGlicko2Scale(this.rating)

  /** Set the average skill value, taking in a value in Glicko2 scale.
    */
  private[impl] def setGlicko2Rating(r: Double) =
    rating = convertRatingToOriginalGlickoScale(r)

  /** Return the rating deviation of the player scaled down to the scale used by the algorithm's internal
    * workings.
    */
  private[impl] def getGlicko2RatingDeviation: Double = convertRatingDeviationToGlicko2Scale(ratingDeviation)

  /** Set the rating deviation, taking in a value in Glicko2 scale.
    */
  private[impl] def setGlicko2RatingDeviation(rd: Double) =
    ratingDeviation = convertRatingDeviationToOriginalGlickoScale(rd)

  /** Used by the calculation engine, to move interim calculations into their "proper" places.
    */
  private[impl] def finaliseRating() =
    setGlicko2Rating(workingRating)
    setGlicko2RatingDeviation(workingRatingDeviation)
    volatility = workingVolatility
    workingRatingDeviation = 0d
    workingRating = 0d
    workingVolatility = 0d

  private[impl] def incrementNumberOfResults(increment: Int) =
    numberOfResults = numberOfResults + increment

  override def toString = f"Rating($rating%1.2f, $ratingDeviation%1.2f, $volatility%1.2f, $numberOfResults)"
