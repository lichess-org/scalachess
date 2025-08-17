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

  override val initialBoard: Board = Board.standard
  override def initialPieces: Map[Square, Piece] = initialBoard.pieceMap

  override def validMoves(position: Position): List[Move] =
    import position.{ genNonKing, genEnPassant, us }
    val targets = ~us
    (genNonKing(targets) ++ genKing(position, targets) ++ genEnPassant(us & position.pawns)).view
      .map(explodeSurroundingPieces)
      .filter(kingSafety)
      .toList

  /** In atomic chess, it is possible to win with a single knight, bishop, etc, by exploding
    * a piece in the opponent's king's proximity. On the other hand, a king alone or a king with
    * immobile pawns is not sufficient material to win with.
    */
  override def hasInsufficientMaterial(position: Position, color: Color): Boolean =
    position.kingsOnlyOf(color)

  /** Atomic chess has a special end where a king has been killed by exploding with an adjacent captured piece */
  override def specialEnd(position: Position): Boolean = position.kings.count < 2
  override def validMovesAt(position: Position, square: Square): List[Move] =
    super.validMovesAt(position, square).view.map(explodeSurroundingPieces).filter(kingSafety).toList

  private def genKing(position: Position, mask: Bitboard) =
    import position.{ genUnsafeKing, genCastling }
    position.ourKing.fold(Nil): king =>
      genCastling(king) ++ genUnsafeKing(king, mask)

  /** In atomic chess, a king cannot be threatened while it is in the perimeter of the other king as were the other player
    * to capture it, their own king would explode. This effectively makes a king invincible while connected with another
    * king.
    */
  override def kingThreatened(board: Board, color: Color): Check = Check:
    import board.{ kingPosOf, kingOf, occupied }
    kingPosOf(color).exists: k =>
      k.kingAttacks.isDisjoint(kingOf(!color)) &&
        attackersWithoutKing(board, occupied, k, !color).nonEmpty

  // moves exploding opponent king are always playable
  override def kingSafety(m: Move): Boolean =
    (kingThreatened(m.afterWithoutHistory.board, m.color).no ||
      explodesOpponentKing(m.before)(m))
      && !explodesOwnKing(m.before)(m)

  override def castleCheckSafeSquare(board: Board, king: Square, color: Color, occupied: Bitboard): Boolean =
    king.kingAttacks.intersects(board.kingOf(!color)) ||
      attackersWithoutKing(board, occupied, king, !color).isEmpty

  /** Move threatens to explode the opponent's king */
  private def explodesOpponentKing(position: Position)(move: Move): Boolean =
    move.captures && (position.them & position.kings).intersects(move.dest.kingAttacks)

  /** Move threatens to illegally explode our own king */
  private def explodesOwnKing(position: Position)(move: Move): Boolean =
    move.captures && (position.us & position.kings).intersects(move.dest.kingAttacks)

  private def attackersWithoutKing(board: Board, occupied: Bitboard, s: Square, attacker: Color): Bitboard =
    import board.{ byColor, rooks, queens, bishops, knights, pawns }
    byColor(attacker) & (
      s.rookAttacks(occupied) & (rooks ^ queens) |
        s.bishopAttacks(occupied) & (bishops ^ queens) |
        s.knightAttacks & knights |
        s.pawnAttacks(!attacker) & pawns
    )

  /** If the move captures, we explode the surrounding pieces. Otherwise, nothing explodes. */
  private def explodeSurroundingPieces(move: Move): Move =
    if move.captures then
      val afterBoard = move.afterWithoutHistory
      // Pawns are immune (for some reason), but all pieces surrounding the captured piece and the capturing piece
      // itself explode
      val squaresToExplode = (move.dest.kingAttacks & afterBoard.occupied & ~afterBoard.pawns) | move.dest.bl
      val afterExplosions = afterBoard.withBoard(afterBoard.board.discard(squaresToExplode))

      val rooksToExploded = squaresToExplode & afterBoard.rooks

      val castles = afterBoard.castles & ~rooksToExploded
      val unMovedRooks = afterBoard.unmovedRooks & ~rooksToExploded
      val newBoard = afterExplosions.updateHistory(_.copy(castles = castles, unmovedRooks = unMovedRooks))
      move.copy(afterWithoutHistory = newBoard)
    else move

  /** Since kings cannot confine each other, if either player has only a king
    * then either a queen or multiple pieces are required for checkmate.
    */
  private def insufficientAtomicWinningMaterial(position: Board): Boolean =
    lazy val bishopsOnOppositeColors = InsufficientMatingMaterial.bishopsOnOppositeColors(position)

    // Bishops of opposite color (no other pieces) endgames are dead drawn
    // except if either player has multiple bishops so a helpmate is possible
    if position.count(White) >= 2 && position.count(Black) >= 2 then
      position.kingsAndBishopsOnly && position.nbPieces <= 4 && bishopsOnOppositeColors

    // Queen, rook + any, bishop + any (same piece color), or 3 knights can mate
    else if position.kingsAndKnightsOnly then position.nbPieces <= 4
    else position.kingsRooksAndMinorsOnly && !bishopsOnOppositeColors && position.nbPieces <= 3

  /*
   * Bishops on opposite coloured squares can never capture each other to cause a king to explode and a traditional
   * mate would be not be very likely. Additionally, a player can only mate another player with sufficient material.
   * We also look out for closed positions (pawns that cannot move and kings which cannot capture them.)
   */
  override def isInsufficientMaterial(position: Position): Boolean =
    insufficientAtomicWinningMaterial(position.board) || atomicClosedPosition(position)

  /** Since a king cannot capture, K + P vs K + P where none of the pawns can move is an automatic draw
    */
  private def atomicClosedPosition(position: Position) =
    val closedStructure = position.pieces.forall((square, piece) =>
      InsufficientMatingMaterial.pawnBlockedByPawn(square, position)
        || piece.is(King) || piece.is(Bishop)
    )
    val randomBishop = position.pieces.find { case (_, piece) => piece.is(Bishop) }
    val bishopsAbsentOrPawnitized = randomBishop match
      case Some((square, piece)) => bishopPawnitized(position.board, piece.color, square.isLight)
      case None => true
    closedStructure && bishopsAbsentOrPawnitized

  private def bishopPawnitized(board: Board, sideWithBishop: Color, bishopLight: Boolean) =
    board.pieceMap.forall((square, piece) =>
      (piece.is(Pawn) && piece.is(sideWithBishop)) ||
        (piece.is(Pawn) && piece.is(!sideWithBishop) && square.isLight == !bishopLight) ||
        (piece.is(Bishop) && piece.is(sideWithBishop) && square.isLight == bishopLight) ||
        piece.is(King)
    )
