package chess

/** Utility methods for helping to determine whether a situation is a draw or a draw
  * on a player flagging.
  *
  * See http://www.e4ec.org/immr.html
  */
object InsufficientMatingMaterial:

  // verify if there are at least two bishops of opposite color
  // no matter which sides they are on
  def bishopsOnOppositeColors(board: Board): Boolean =
    board.bishops.map(_.isLight).distinct.size == 2

  /*
   * Returns true if a pawn cannot progress forward because it is blocked by a pawn
   * and it doesn't have any capture
   */
  def pawnBlockedByPawn(pawn: Square, board: Board): Boolean =
    board(pawn).exists(p =>
      p.is(Pawn) &&
        Situation(board, p.color).generateMovesAt(pawn).isEmpty && {
          val blockingPosition = posAheadOfPawn(pawn, p.color)
          blockingPosition.flatMap(board(_)).exists(_.is(Pawn))
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
   */
  def apply(board: Board, color: Color): Boolean =
    if board.kingsOnlyOf(color) then true
    else if board.kingsAndKnightsOnlyOf(color) then
      board.nonKingsOf(color).count == 1 &&
      board.onlyOf(!color, board.kings | board.queens)
    else if board.kingsAndBishopsOnlyOf(color) then
      !(bishopsOnOppositeColors(board) || (board(!color, Knight) | board(!color, Pawn)).nonEmpty)
    else false

  /** Determines the position one ahead of a pawn based on the color of the piece.
    * White pawns move up and black pawns move down.
    */
  def posAheadOfPawn(square: Square, color: Color): Option[Square] = color.fold(square.up, square.down)
