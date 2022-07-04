package chess
package variant

case object Fluidity
    extends Variant(
      id = 10,
      key = "fluidity",
      name = "Fluidity",
      shortName = "Fluid",
      title = "Dissect the opponent's pieces, and checkmate or dissect the king.",
      standardInitialPosition = false
    ) {

  def pieces = Standard.pieces

  override def hasMoveEffects = true

  /** Move threatens to dissect the opponent's king */
  private def dissectsOpponentKing(situation: Situation)(move: Move): Boolean =
    move.captures && {
      situation.board.kingPosOf(!situation.color) exists move.dest.touches
    }

  /** Move threatens to illegally dissect our own king */
  private def explodesOwnKing(situation: Situation)(move: Move): Boolean = {
    move.captures && (situation.kingPos exists move.dest.touches)
  }

  private def (board: Board, to: Pos, color: Color): Boolean =
    board.kingPosOf(color) exists to.touches

  /** In fluidity chess, a king can be threatened while it is in the perimeter of the other king.
    */
  override def kingThreatened(
      board: Board,
      color: Color,
      to: Pos,
      filter: Piece => Boolean = _ => true
  ): Boolean = {
    board.pieces exists {
      case (pos, piece)
          if piece.color == color && filter(piece) && piece.eyes(pos, to) && (
            board,
            to,
            color
          ) =>
        (!piece.role.projection) || piece.role.dir(pos, to).exists {
          longRangeThreatens(board, pos, _, to)
        }
      case _ => false
    }
  }

  // moves dissecting opponent king are always playable
  override def kingSafety(m: Move, filter: Piece => Boolean, kingPos: Option[Pos]): Boolean = {
    !kingPos.exists(kingThreatened(m.after, !m.color, _, filter)) ||
    explodesOpponentKing(m.situationBefore)(m)
  } && !explodesOwnKing(m.situationBefore)(m)

  /** If the move captures, we explode the surrounding pieces. Otherwise, nothing explodes. */
  private def explodeSurroundingPieces(move: Move): Move = {
    if (move.captures) {
      val affectedPos = surroundingPositions(move.dest)
      val afterBoard  = move.after
      val destination = move.dest

      val boardPieces = afterBoard.pieces

    } else move
  }

  /** The positions surrounding a given position on the board. Any square at the edge of the board has
    * less surrounding positions than the usual eight.
    */
  private[chess] def surroundingPositions(pos: Pos): Set[Pos] =
    Set(pos.up, pos.down, pos.left, pos.right, pos.upLeft, pos.upRight, pos.downLeft, pos.downRight).flatten

  override def addVariantEffect(move: Move): Move = capturePassingPieces(move)

  /** Since kings cannot confine each other, if either player has only a king
    * then either a queen or multiple pieces are required for checkmate.
    */
  private def insufficientFludityWinningMaterial(board: Board) = {
    val kingsAndBishopsOnly = board.pieces forall { p =>
      (p._2 is King) || (p._2 is Bishop)
    }
    lazy val bishopsOnOppositeColors = InsufficientMatingMaterial.bishopsOnOppositeColors(board)
    lazy val kingsAndKnightsOnly = board.pieces forall { p =>
      (p._2 is King) || (p._2 is Knight)
    }
    lazy val kingsRooksAndMinorsOnly = board.pieces forall { p =>
      (p._2 is King) || (p._2 is Rook) || (p._2 is Bishop) || (p._2 is Knight)
    }

    // Bishops of opposite color (no other pieces) endgames are dead drawn
    // except if either player has multiple bishops so a helpmate is possible
    if (board.count(White) >= 2 && board.count(Black) >= 2)
      kingsAndBishopsOnly && board.pieces.size <= 4 && bishopsOnOppositeColors

    // Queen, rook + any, bishop + any (same piece color), or 2 knights can mate
    else if (kingsAndKnightsOnly) board.pieces.size <= 4
    else kingsRooksAndMinorsOnly && !bishopsOnOppositeColors && board.pieces.size <= 3
  }

  /*
   * Bishops on opposite coloured squares can never capture each other to dissect a king and a traditional
   * mate would be not be very likely. Additionally, a player can only mate another player with sufficient material.
   */
  override def isInsufficientMaterial(board: Board) = {
    insufficientFluidityWinningMaterial(board)
  }

  /** In fluidity chess, it is possible to win with a single knight, bishop, etc, by dissecting
    * the king. On the other hand, a king alone
    * is not sufficient material to win with.
    */
  override def opponentHasInsufficientMaterial(situation: Situation) =
    situation.board.rolesOf(!situation.color) == List(King)

  /** Fluidity chess has a special end where a king has been killed by dissecting */
  override def specialEnd(situation: Situation) = situation.board.kingPos.size != 2
}
