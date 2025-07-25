package chess.rating
package glicko

import chess.{ ByColor, IntRating, Outcome }
import scalalib.newtypes.OpaqueDouble

import java.time.Instant

case class Glicko(
    rating: Double,
    deviation: Double,
    volatility: Double
):
  def intRating: IntRating = IntRating(rating.toInt)
  def intDeviation = deviation.toInt
  def provisional = RatingProvisional(deviation >= provisionalDeviation)
  def established = provisional.no
  def establishedIntRating = Option.when(established)(intRating)
  def clueless = deviation >= cluelessDeviation
  def display = s"$intRating${if provisional.yes then "?" else ""}"
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

val provisionalDeviation = 110
val cluelessDeviation = 230

case class Player(
    glicko: Glicko,
    numberOfResults: Int,
    lastRatingPeriodEnd: Option[Instant] = None
):
  export glicko.*

case class Game(players: ByColor[Player], outcome: Outcome)

opaque type Tau = Double
object Tau extends OpaqueDouble[Tau]:
  val default: Tau = 0.75d

opaque type RatingPeriodsPerDay = Double
object RatingPeriodsPerDay extends OpaqueDouble[RatingPeriodsPerDay]:
  val default: RatingPeriodsPerDay = 0d
