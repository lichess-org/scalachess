package chess

/**
 * Utility methods for helping to determine whether a situation is a draw or a draw
 * on a player flagging.
 *
 * See http://www.e4ec.org/immr.html
 */
object InsufficientMatingMaterial {

  def nonKingPieceMap(board: Board) = board.pieces.filter(_._2.role != King)
  def nonKingNonBishopPieceMap(board: Board) = board.pieces.filter(p => (p._2.role != King && p._2.role != Bishop))
  def nonKingPieces(board: Board) = nonKingPieceMap(board).toList
  def boardWithNonKingPieces(board: Board) = board withPieces nonKingPieceMap(board)
  def boardWithNonKingNonBishopPieces(board: Board) = board withPieces nonKingNonBishopPieceMap(board)

  /**
   * Returns true when only the bishops are mobile and the bishops cannot capture.
   * Assumes bishops can move through friendly pieces.
   */
  def bishopsCannotCapture(board: Board) = mate(boardWithNonKingNonBishopPieces(board)) && {
    val notKingPieces = nonKingPieces(board)
    val whitePlayerPieces = notKingPieces.filter(_._2.color == Color.White)
    val whitePlayerBishops = whitePlayerPieces.filter(_._2.role == Bishop)
    val blackPlayerPieces = notKingPieces.filter(_._2.color == Color.Black)
    val blackPlayerBishops = blackPlayerPieces.filter(_._2.role == Bishop)

    !whitePlayerBishops.exists {
      case (pos, _) => blackPlayerPieces.exists(_._1.color == pos.color)
    } && !blackPlayerBishops.exists {
      case (pos, _) => whitePlayerPieces.exists(_._1.color == pos.color)
    }
  }

  /**
   * Returns true when the only non-king pieces that remain are bishops that cannot checkmate.
   */
  def bishopsCannotCheckmate(board: Board) = {
    val notKingPieces = nonKingPieces(board)
    val onlyBishopsRemain = !notKingPieces.exists(_._2.role != Bishop)

    def piecesOnSameColor  = notKingPieces.map(_._1.color).distinct.size == 1
    def piecesAreSameColor = notKingPieces.map(_._2.color).distinct.size == 1

    if (onlyBishopsRemain && piecesAreSameColor) notKingPieces.size < 3 || piecesOnSameColor
    else bishopsCannotCapture(board)
  }

  /**
   * Returns true if a pawn cannot progress forward because it is blocked by a pawn
   */
  def pawnBlockedByPawn(pawn: Actor, board: Board) = pawn.moves.isEmpty && {
    val blockingPosition = Actor.posAheadOfPawn(pawn.pos, pawn.piece.color)
    blockingPosition.flatMap(board.actorAt(_)).exists(_.piece.is(Pawn))
  }

  /**
   * Returns true if no pieces can move (checkmate or stalemate)
   */
  def mate(board: Board) = board.actors.values.forall(actor => actor.moves.isEmpty)

  /**
   * Determines whether a board position is an automatic draw due to neither player
   * being able to mate the other as informed by the traditional chess rules.
   */
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
      case Nil => true
      case List(Knight) => board rolesOf !color filter (King !=) filter (Queen !=) isEmpty
      case List(Bishop) => board rolesOf !color filter (King !=) filter (Queen !=) filter (Rook !=) isEmpty
      case _ => false
    }
}
