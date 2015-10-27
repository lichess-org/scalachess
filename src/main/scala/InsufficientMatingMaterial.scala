package chess

// http://www.e4ec.org/immr.html

object InsufficientMatingMaterial {

  def nonKingPieces(board: Board) = board.pieces.filter(_._2.role != King).toList

  /**
   * Returns true when the only non-king pieces that remain are bishops that cannot
   * capture each other.
   */
  def bishopsOnDifferentColor(board: Board) = {
    val notKingPieces = nonKingPieces(board)
    val onlyBishopsRemain = !notKingPieces.exists(_._2.role != Bishop)

    if (!onlyBishopsRemain) {
        false
    }
    else {
        val whitePlayerBishops = notKingPieces.filter(_._2.color == Color.White)
        val blackPlayerBishops = notKingPieces.filter(_._2.color == Color.Black)

        !whitePlayerBishops.exists {
            case (pos, _) =>
                val squareColor = pos.color
                blackPlayerBishops.exists(_._1.color == squareColor)
        }
    }
  }

  def apply(board: Board) = {

    lazy val notKingPieces = nonKingPieces(board)

    def kingsOnly = board.pieces forall { _._2 is King }

    def bishopsOnSameColor =
      notKingPieces.map(_._2.role).distinct == List(Bishop) &&
        notKingPieces.map(_._1.color).distinct.size == 1

    def singleKnight = notKingPieces.map(_._2.role) == List(Knight)

    kingsOnly || bishopsOnSameColor || singleKnight
  }

  def apply(board: Board, color: Color) =
    board rolesOf color filter (King !=) match {
      case Nil | List(Knight) | List(Bishop) => true
      case _                                 => false
    }
}
