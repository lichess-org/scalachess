package chess

/**
 * Utility methods for helping to determine whether a situation is a draw or a draw
 * on a player flagging.
 *
 * See http://www.e4ec.org/immr.html
 */
object InsufficientMatingMaterial {

  def nonKingPieces(board: Board) = board.pieces filter (_._2.role != King)

  def bishopsOnOppositeColors(board: Board) =
    (board.pieces collect { case (pos, Piece(_, Bishop)) => pos.color } toList).distinct.size == 2

  def piecesOnOppositeColors(pieces: PieceMap) =
    (pieces map { _._1.color } toList).distinct.size == 2

  def bishopCanCaptureNonKing(board: Board) = {
    board.pieces exists { b => (b._2 is Bishop) && (board.pieces exists { p => p._2.color != b._2.color && !p._2.is(King) && p._1.color == b._1.color }) }
  }

  /*
   * Returns true if a pawn cannot progress forward because it is blocked by a pawn
   */
  def pawnBlockedByPawn(pawn: Actor, board: Board) =
    pawn.moves.isEmpty && (Actor.posAheadOfPawn(pawn.pos, pawn.piece.color) match {
      case Some(pos) => board.pieces(pos) is Pawn
      case _ => false
    })

  /*
   * Determines whether a color (of a game in progress) has mating material
   */
  def apply(game: Game, color: Color): Boolean = {
    if (game.situation.color != color) game.situation.insufficientWinningMaterial
    else {
      // Crazyhouse drop moves are not accounted for in game.situation.moves
      val moves = game.situation.moves;
      (!(moves isEmpty)) && (moves.values forall { _ forall { move => game.apply(move).situation.insufficientWinningMaterial } })
    }
  }
}
