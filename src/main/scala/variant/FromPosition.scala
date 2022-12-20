package chess
package variant

case object FromPosition
    extends Variant(
      id = Variant.Id(3),
      key = Variant.Key("fromPosition"),
      uciKey = "chess",
      name = "From Position",
      shortName = "FEN",
      title = "Custom starting position",
      standardInitialPosition = false
    ):

  def pieces = Standard.pieces
