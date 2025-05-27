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

  override val pieces: Map[Square, Piece] = Standard.pieces
  override val board: Board               = Board.fromMap(pieces)

  override def validMoves(position: Position): List[Move] =
    Standard.validMoves(position)

  override def validMovesAt(position: Position, square: Square): List[Move] =
    super.validMovesAt(position, square).filter(kingSafety)
