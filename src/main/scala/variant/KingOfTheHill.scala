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

  override def valid(situation: Situation, strict: Boolean): Boolean = Standard.valid(situation, strict)

  // E4, D4, E5, D5
  private val center = 0x1818000000L

  override def specialEnd(situation: Situation) =
    situation.kingOf(!situation.color).intersects(center)

  /** You only need a king to be able to win in this variant
    */
  override def opponentHasInsufficientMaterial(situation: Situation) = false
  override def isInsufficientMaterial(board: Board)                  = false
