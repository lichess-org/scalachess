package chess

object Setup {

  def apply(variant: chess.variant.Variant): Game = Game(
    board = Board(pieces = variant.pieces, variant = variant)
  )
}
