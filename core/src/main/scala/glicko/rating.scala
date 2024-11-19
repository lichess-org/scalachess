package chess
package glicko // todo move to chess.rating?

import alleycats.Zero

opaque type IntRating = Int
object IntRating extends RichOpaqueInt[IntRating]:
  extension (r: IntRating) def applyDiff(diff: IntRatingDiff): IntRating = r + diff.value

opaque type IntRatingDiff = Int
object IntRatingDiff extends RichOpaqueInt[IntRatingDiff]:
  extension (diff: IntRatingDiff)
    def positive: Boolean = diff > 0
    def negative: Boolean = diff < 0
    def zero: Boolean     = diff == 0
  given Zero[IntRatingDiff] = Zero(0)

opaque type Rating = Double
object Rating extends OpaqueDouble[Rating]

opaque type RatingProvisional = Boolean
object RatingProvisional extends YesNo[RatingProvisional]
