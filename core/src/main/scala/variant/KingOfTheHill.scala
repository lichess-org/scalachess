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

  def pieces = Standard.pieces

  def validMoves(board: Position): List[Move] =
    Standard.validMoves(board)

  override def valid(board: Position, strict: Boolean): Boolean =
    Standard.valid(board, strict)

  override def specialEnd(board: Position): Boolean =
    board.kingOf(!board.color).intersects(Bitboard.center)

  /** You only need a king to be able to win in this variant
    */
  override def opponentHasInsufficientMaterial(board: Position): Boolean = false
  override def isInsufficientMaterial(board: Position): Boolean          = false
