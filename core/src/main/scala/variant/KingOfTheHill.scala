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

  def validMoves(board: Board): List[Move] =
    Standard.validMoves(board)

  override def valid(board: Board, strict: Boolean): Boolean = Standard.valid(board, strict)

  override def specialEnd(board: Board) =
    board.kingOf(!board.color).intersects(bitboard.Bitboard.center)

  /** You only need a king to be able to win in this variant
    */
  override def opponentHasInsufficientMaterial(board: Board): Boolean = false
  override def isInsufficientMaterial(board: Board): Boolean          = false
