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

  def validMoves(situation: Situation): List[Move] =
    Standard.validMoves(situation)

  override def valid(situation: Board, strict: Boolean): Boolean = Standard.valid(situation, strict)

  override def specialEnd(situation: Board) =
    situation.kingOf(!situation.color).intersects(bitboard.Bitboard.center)

  /** You only need a king to be able to win in this variant
    */
  override def opponentHasInsufficientMaterial(situation: Board) = false
  override def isInsufficientMaterial(board: Board)              = false
