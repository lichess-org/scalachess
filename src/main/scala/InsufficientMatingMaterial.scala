package chess

/**
 * Utility methods for helping to determine whether a situation is a draw or a draw
 * on a player flagging.
 *
 * See http://www.e4ec.org/immr.html
 */
object InsufficientMatingMaterial {

  def nonKingPieceMap(board: Board) = board.pieces.filter {case (_, piece) => piece isNot King}
  def nonKingNonBishopPieceMap(board: Board) = board.pieces.filter {case (_, piece) => (piece isNot King) && (piece isNot Bishop)}
  def nonKingPieces(board: Board) = nonKingPieceMap(board).toList
  def boardWithNonKingPieces(board: Board) = board withPieces nonKingPieceMap(board)
  def boardWithNonKingNonBishopPieces(board: Board) = board withPieces nonKingNonBishopPieceMap(board)

  /**
   * Returns true when only the bishops are mobile and the bishops cannot capture.
   * Assumes bishops can move through friendly pieces.
   */
  def bishopsCannotCapture(board: Board) = noPieceMoves(boardWithNonKingNonBishopPieces(board)) && {
    val notKingPieces = nonKingPieces(board)
    val whitePlayerPieces = notKingPieces.filter {case (_, piece) => piece is Color.White}
    val whitePlayerBishops = whitePlayerPieces.filter {case (_, piece) => piece is Bishop}
    val blackPlayerPieces = notKingPieces.filter {case (_, piece) => piece is Color.Black}
    val blackPlayerBishops = blackPlayerPieces.filter {case (_, piece) => piece is Bishop}

    !whitePlayerBishops.map {case (pos, _) => pos.color}.exists {
      case color => blackPlayerPieces.exists {case (pos, _) => pos.color == color}
    } && !blackPlayerBishops.map {case (pos, _) => pos.color}.exists {
      case color => whitePlayerPieces.exists {case (pos, _) => pos.color == color}
    }
  }

  /**
   * Returns true when the only non-king pieces that remain are bishops that cannot checkmate.
   */
  def bishopsCannotCheckmate(board: Board) = {
    val notKingPieces = nonKingPieces(board)
    val onlyBishopsRemain = nonKingNonBishopPieceMap(board).isEmpty
    lazy val piecesOnSameColor  = notKingPieces.map {case (pos, _) => pos.color}.distinct.size < 2
    lazy val piecesAreSameColor = notKingPieces.map {case (_, piece) => piece.color}.distinct.size < 2

    if (onlyBishopsRemain && piecesAreSameColor) piecesOnSameColor
    else if (onlyBishopsRemain && !piecesOnSameColor) notKingPieces.size < 3
    else bishopsCannotCapture(board)
  }

  /**
   * Returns true when the only non-king pieces of a color that remain are bishops that cannot checkmate.
   */
  def bishopsCannotCheckmate(board: Board, color: Color) = {
    val notKingPieces = nonKingPieces(board)
    lazy val opponentPieces = notKingPieces.filter {case (_, piece) => piece.color != color}
    val onlyBishopsRemain = nonKingNonBishopPieceMap(board).isEmpty
    lazy val piecesOnSameColor  = notKingPieces.map {case (pos, _) => pos.color}.distinct.size < 2
    lazy val piecesAreSameColor = notKingPieces.map {case (_, piece) => piece.color}.distinct.size < 2

    if (onlyBishopsRemain && piecesAreSameColor) piecesOnSameColor
    else opponentPieces.size >= 2 || bishopsCannotCapture(board)
  }

  /**
   * Returns true if a pawn cannot progress forward because it is blocked by a pawn
   */
  def pawnBlockedByPawn(pawn: Actor, board: Board) = pawn.moves.isEmpty && {
    val blockingPosition = Actor.posAheadOfPawn(pawn.pos, pawn.piece.color)
    blockingPosition.flatMap(board.actorAt(_)).exists(_.piece.is(Pawn))
  }

  /**
   * Returns true if no pieces can move
   */
  def noPieceMoves(board: Board) = board.actors.values.forall(_.moves.isEmpty)

  /**
   * Determines whether a board position is an automatic draw due to neither player
   * being able to mate the other as informed by the traditional chess rules.
   */
  def apply(board: Board) = {

    lazy val notKingPieces = nonKingPieces(board)

    val kingsOnly = board.pieces forall { case (_, piece) => piece is King }

    lazy val bishopsOnSameColor =
      notKingPieces.map {case (_, piece) => piece.role}.distinct == List(Bishop) &&
        notKingPieces.map {case (pos, _) => pos.color}.distinct.size == 1

    lazy val singleKnight = notKingPieces.map {case (_, piece) => piece.role} == List(Knight)

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
