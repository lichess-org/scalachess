package chess

/** Utility methods for helping to determine whether a board is a draw or a draw
  * on a player flagging.
  *
  * See http://www.e4ec.org/immr.html
  */
object InsufficientMatingMaterial:

  // verify if there are at least two bishops of opposite color
  // no matter which sides they are on
  def bishopsOnOppositeColors(board: Board): Boolean =
    board.bishops.intersects(Bitboard.lightSquares) &&
      board.bishops.intersects(Bitboard.darkSquares)

  /*
   * Returns true if a pawn cannot progress forward because it is blocked by a pawn
   * and it doesn't have any capture
   */
  def pawnBlockedByPawn(pawn: Square, position: Position): Boolean =
    position
      .pieceAt(pawn)
      .exists(p =>
        p.is(Pawn) &&
          position.withColor(p.color).generateMovesAt(pawn).isEmpty && {
            val blockingPosition = pawn.nextRank(p.color)
            blockingPosition.flatMap(position.pieceAt).exists(_.is(Pawn))
          }
      )

  /*
   * Determines whether a board position is an automatic draw due to neither player
   * being able to mate the other as informed by the traditional chess rules.
   */
  def apply(board: Board): Boolean =
    board.kingsAndMinorsOnly &&
      (board.nbPieces <= 3 || (board.kingsAndBishopsOnly && !bishopsOnOppositeColors(board)))

  /*
   * Determines whether a color does not have mating material. In general:
   * King by itself is not mating material
   * King + knight mates against king + any(rook, bishop, knight, pawn)
   * King + bishop mates against king + any(bishop, knight, pawn)
   * King + bishop(s) versus king + bishop(s) depends upon bishop square colors
   * So this function returns true in three cases:
   * - if color has only king
   * - if color has king + knight and opponent has king + queen(s)
   * - if color has king + bishop and opponent doesn't have:
   *   - opposite color bishop(s)
   *   - or knight(s) or pawn(s)
   */
  def apply(board: Board, color: Color): Boolean =
    import board.*
    inline def onlyKing = kingsOnlyOf(color)
    inline def KN =
      onlyOf(color, King, Knight) && count(color, Knight) == 1 && onlyOf(!color, King, Queen)
    inline def KB =
      onlyOf(color, King, Bishop) &&
        !(bishopsOnOppositeColors(board) || byPiece(!color, Knight, Pawn).nonEmpty)

    onlyKing || KN || KB
