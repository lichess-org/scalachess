package chess

// http://www.e4ec.org/immr.html

object InsufficientMatingMaterial {

  def apply(board: Board) = {

    def kingsOnly = board.pieces.size < 3

    def bishopsOnSameColor =
      notKingPieces.map(_._2.role).distinct == List(Bishop) &&
        notKingPieces.map(_._1.color).distinct.size == 1

    def singleKnight = notKingPieces.map(_._2.role) == List(Knight)

    lazy val notKingPieces = board.pieces.filter(_._2.role != King).toList

    kingsOnly || bishopsOnSameColor || singleKnight
  }

  def apply(board: Board, color: Color) = 
    board rolesOf color filter (King !=) match {
      case roles if roles.size > 1 => true
      case Nil | List(Knight) | List(Bishop) => false
      case _                       => true
    }
}
