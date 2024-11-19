package chess.glicko

import java.time.Instant

case class Player(
    rating: Double,
    ratingDeviation: Double,
    volatility: Double,
    numberOfResults: Int,
    lastRatingPeriodEnd: Option[Instant] = None
)
