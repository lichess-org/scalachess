package chess

// http://www.e4ec.org/immr.html

case class InsufficientMatingMaterial(board: Board) {

  def apply = kingsOnly || bishopsOnSameColor || singleKnight

  private def kingsOnly = board.pieces.size < 3

  private def bishopsOnSameColor = 
    notKingPieces.map(_._2.role).distinct == List(Bishop) &&
    notKingPieces.map(_._1.color).distinct.size == 1

  private def singleKnight = notKingPieces.map(_._2.role) == List(Knight)

  private lazy val notKingPieces = 
    board.pieces.filter(_._2.role != King).toList
}
