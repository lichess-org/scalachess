package chess
package variant

case object Atomic
    extends Variant(
      id = Variant.Id(7),
      key = Variant.LilaKey("atomic"),
      uciKey = Variant.UciKey("atomic"),
      name = "Atomic",
      shortName = "Atom",
      title = "Nuke your opponent's king to win.",
      standardInitialPosition = true
    ):

  override val pieces: Map[Square, Piece] = Standard.pieces

  def validMoves(board: Position): List[Move] =
    import board.{ genNonKing, genEnPassant, us }
    val targets = ~us
    val moves   = genNonKing(targets) ++ genKing(board, targets) ++ genEnPassant(us & board.pawns)
    moves.map(explodeSurroundingPieces).filter(kingSafety)

  private def genKing(board: Position, mask: Bitboard) =
    import board.{ genUnsafeKing, genCastling }
    board.ourKing.fold(Nil): king =>
      genCastling(king) ++ genUnsafeKing(king, mask)

  /** Move threatens to explode the opponent's king */
  private def explodesOpponentKing(board: Position)(move: Move): Boolean =
    move.captures && (board.them & board.kings).intersects(move.dest.kingAttacks)

  /** Move threatens to illegally explode our own king */
  private def explodesOwnKing(board: Position)(move: Move): Boolean =
    move.captures && (board.us & board.kings).intersects(move.dest.kingAttacks)

  /** In atomic chess, a king cannot be threatened while it is in the perimeter of the other king as were the other player
    * to capture it, their own king would explode. This effectively makes a king invincible while connected with another
    * king.
    */
  override def kingThreatened(board: Board, color: Color): Check = Check:
    import board.{ kingPosOf, kingOf, occupied }
    kingPosOf(color).exists: k =>
      k.kingAttacks.isDisjoint(kingOf(!color)) &&
        attackersWithoutKing(board, occupied, k, !color).nonEmpty

  private def attackersWithoutKing(board: Board, occupied: Bitboard, s: Square, attacker: Color): Bitboard =
    import board.{ byColor, rooks, queens, bishops, knights, pawns }
    byColor(attacker) & (
      s.rookAttacks(occupied) & (rooks ^ queens) |
        s.bishopAttacks(occupied) & (bishops ^ queens) |
        s.knightAttacks & knights |
        s.pawnAttacks(!attacker) & pawns
    )

  // moves exploding opponent king are always playable
  override def kingSafety(m: Move): Boolean =
    (kingThreatened(m.after.board, m.color).no ||
      explodesOpponentKing(m.boardBefore)(m))
      && !explodesOwnKing(m.boardBefore)(m)

  override def castleCheckSafeSquare(
      board: Position,
      king: Square,
      color: Color,
      occupied: Bitboard
  ): Boolean =
    king.kingAttacks.intersects(board.kingOf(!color)) ||
      attackersWithoutKing(board.board, occupied, king, !color).isEmpty

  /** If the move captures, we explode the surrounding pieces. Otherwise, nothing explodes. */
  def explodeSurroundingPieces(move: Move): Move =
    if move.captures then
      val afterBoard = move.after
      // Pawns are immune (for some reason), but all pieces surrounding the captured piece and the capturing piece
      // itself explode
      val squaresToExplode = (move.dest.kingAttacks & afterBoard.occupied & ~afterBoard.pawns) | move.dest.bl
      val afterExplosions  = afterBoard.withBoard(afterBoard.board.discard(squaresToExplode))

      val rooksToExploded = squaresToExplode & afterBoard.rooks

      val castles      = afterBoard.castles & ~rooksToExploded
      val unMovedRooks = afterBoard.unmovedRooks & ~rooksToExploded
      val newBoard     = afterExplosions.updateHistory(_.copy(castles = castles, unmovedRooks = unMovedRooks))
      move.withAfter(newBoard)
    else move

  /** Since kings cannot confine each other, if either player has only a king
    * then either a queen or multiple pieces are required for checkmate.
    */
  private def insufficientAtomicWinningMaterial(board: Position): Boolean =
    lazy val bishopsOnOppositeColors = InsufficientMatingMaterial.bishopsOnOppositeColors(board)

    // Bishops of opposite color (no other pieces) endgames are dead drawn
    // except if either player has multiple bishops so a helpmate is possible
    if board.count(White) >= 2 && board.count(Black) >= 2 then
      board.kingsAndBishopsOnly && board.nbPieces <= 4 && bishopsOnOppositeColors

    // Queen, rook + any, bishop + any (same piece color), or 3 knights can mate
    else if board.kingsAndKnightsOnly then board.nbPieces <= 4
    else board.kingsRooksAndMinorsOnly && !bishopsOnOppositeColors && board.nbPieces <= 3

  /*
   * Bishops on opposite coloured squares can never capture each other to cause a king to explode and a traditional
   * mate would be not be very likely. Additionally, a player can only mate another player with sufficient material.
   * We also look out for closed positions (pawns that cannot move and kings which cannot capture them.)
   */
  override def isInsufficientMaterial(board: Position): Boolean =
    insufficientAtomicWinningMaterial(board) || atomicClosedPosition(board)

  /** Since a king cannot capture, K + P vs K + P where none of the pawns can move is an automatic draw
    */
  private def atomicClosedPosition(board: Position) =
    val closedStructure = board.pieces.forall((square, piece) =>
      InsufficientMatingMaterial.pawnBlockedByPawn(square, board)
        || piece.is(King) || piece.is(Bishop)
    )
    val randomBishop = board.pieces.find { case (_, piece) => piece.is(Bishop) }
    val bishopsAbsentOrPawnitized = randomBishop match
      case Some((square, piece)) => bishopPawnitized(board, piece.color, square.isLight)
      case None                  => true
    closedStructure && bishopsAbsentOrPawnitized

  private def bishopPawnitized(board: Position, sideWithBishop: Color, bishopLight: Boolean) =
    board.pieces.forall((square, piece) =>
      (piece.is(Pawn) && piece.is(sideWithBishop)) ||
        (piece.is(Pawn) && piece.is(!sideWithBishop) && square.isLight == !bishopLight) ||
        (piece.is(Bishop) && piece.is(sideWithBishop) && square.isLight == bishopLight) ||
        piece.is(King)
    )

  /** In atomic chess, it is possible to win with a single knight, bishop, etc, by exploding
    * a piece in the opponent's king's proximity. On the other hand, a king alone or a king with
    * immobile pawns is not sufficient material to win with.
    */
  override def opponentHasInsufficientMaterial(board: Position) =
    board.kingsOnlyOf(!board.color)

  /** Atomic chess has a special end where a king has been killed by exploding with an adjacent captured piece */
  override def specialEnd(board: Position): Boolean = board.kings.count < 2
