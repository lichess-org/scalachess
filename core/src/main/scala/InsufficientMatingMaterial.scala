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

  /**
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

  /**
   * Returns whether some square in `destinations` can be reached by a king moving from `startSquare`,
   * while avoiding all squares in `forbidden`.
   *
   * `destinations` must not contain `startSquare`, or any square in `forbidden`.
   */
  def kingPathExists(startSquare: Square, destinations: Bitboard, forbidden: Bitboard): Boolean =
    if destinations.intersects(forbidden) || destinations.contains(startSquare) then
      throw IllegalArgumentException(
        "`destinations` contains either the start square or some forbidden square"
      )

    var skip = forbidden
    var frontier = startSquare.bb

    while frontier.nonEmpty do
      if frontier.intersects(destinations) then return true
      skip |= frontier
      frontier = frontier.fold(Bitboard.empty) { (newFrontier, sq) =>
        newFrontier | (sq.kingAttacks & ~skip)
      }
    end while

    false

  def baseChainPawns(board: Board, color: Color): Board = ???
  /* todo:
    1) Get bitboard of pawns for `color`
    2) Use squares attacked by pawns to find just pawns that are undefended by fellow pawns
    3) Return that bitboard?
   */

  /**
    * Checks if all pawns are locked, just with respect to each other. Other pieces that could allow the
    * pawns to make captures are not considered.
    */
  def allPawnsLocked(board: Board): Boolean =
    List(White, Black).forall: color =>
      board.squaresAttackedByPawns(color).isDisjoint(board.byPiece(!color, Pawn)) &&
        board
          .byPiece(color, Pawn)
          .forall: pawnSq =>
            pawnSq
              .nextRank(color)
              .exists: frontSq =>
                board.pawns.contains(frontSq)

  def onlyKingsAndPawns(board: Board): Boolean = (board.kings | board.pawns) == board.occupied

  def kingPawnFortress(position: Position): Boolean =
    onlyKingsAndPawns(position.board) &&
      allPawnsLocked(position.board) &&
      // todo - add call checking for king path to any base pawns
      ??? &&
      position.enPassantSquare.isEmpty

  /**
   * Determines whether a board position is an automatic draw due to neither player
   * being able to mate the other as informed by the traditional chess rules.
   */
  def apply(position: Position): Boolean =
    val board = position.board
    board.kingsAndMinorsOnly &&
    (board.nbPieces <= 3 || (board.kingsAndBishopsOnly && !bishopsOnOppositeColors(board)))

  /**
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
  def apply(position: Position, color: Color): Boolean =
    import board.*
    val board = position.board
    inline def onlyKing = kingsOnlyOf(color)
    inline def KN =
      onlyOf(color, King, Knight) && count(color, Knight) == 1 && onlyOf(!color, King, Queen)
    inline def KB =
      onlyOf(color, King, Bishop) &&
        !(bishopsOnOppositeColors(board) || byPiece(!color, Knight, Pawn).nonEmpty)

    onlyKing || KN || KB
