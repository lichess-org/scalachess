package chess

object Setup {

  def apply(variant: chess.variant.Variant): Game = Game(
    board = Board(pieces = variant.pieces, castles = variant.castles, variant = variant)
  )
}
