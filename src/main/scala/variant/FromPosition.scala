package chess
package variant

case object FromPosition
    extends Variant(
      id = Variant.Id(3),
      key = Variant.LilaKey("fromPosition"),
      uciKey = Variant.UciKey("chess"),
      name = "From Position",
      shortName = "FEN",
      title = "Custom starting position",
      standardInitialPosition = false
    ):

  def pieces = Standard.pieces

  override def hasValidCheckers(strict: Boolean, situation: Situation): Boolean =
    Standard.hasValidCheckers(strict, situation)
  def validMoves(situation: Situation): List[Move] =
    Standard.validMoves(situation)
