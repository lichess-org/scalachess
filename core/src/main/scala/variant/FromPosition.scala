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

  override val initialPieces: Map[Square, Piece] = Standard.initialPieces
  override val initialBoard: Board               = Board.fromMap(initialPieces)

  override def validMoves(position: Position): List[Move] =
    Standard.validMoves(position)

  override def validMovesAt(position: Position, square: Square): List[Move] =
    super.validMovesAt(position, square).filter(kingSafety)
