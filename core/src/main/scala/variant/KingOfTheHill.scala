package chess
package variant

case object KingOfTheHill
    extends Variant(
      id = Variant.Id(4),
      key = Variant.LilaKey("kingOfTheHill"),
      uciKey = Variant.UciKey("kingofthehill"),
      name = "King of the Hill",
      shortName = "KotH",
      title = "Bring your King to the center to win the game.",
      standardInitialPosition = true
    ):

  override val pieces: Map[Square, Piece] = Standard.pieces

  override def validMoves(position: Position): List[Move] =
    Standard.validMoves(position)

  override def validMovesAt(position: Position, square: Square): List[Move] =
    super.validMovesAt(position, square).filter(kingSafety)

  override def valid(position: Position, strict: Boolean): Boolean =
    Standard.valid(position, strict)

  override def specialEnd(position: Position): Boolean =
    position.kingOf(!position.color).intersects(Bitboard.center)

  /** You only need a king to be able to win in this variant
    */
  override def opponentHasInsufficientMaterial(position: Position): Boolean = false
  override def isInsufficientMaterial(position: Position): Boolean          = false
