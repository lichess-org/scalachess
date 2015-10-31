package chess
package variant

case object Antichess extends Variant(
  id = 6,
  key = "antichess",
  name = "Antichess",
  shortName = "Anti",
  title = "Lose all your pieces to win the game",
  standardInitialPosition = true) {

  // In antichess, it is not permitted to castle
  override def allowsCastling = false

  // In antichess, the king can't be put into check so we always return false
  override def kingThreatened(board: Board, color: Color, to: Pos, filter: Piece => Boolean = _ => true) = false

  // In this variant, a player must capture if a capturing move is available
  override def validMoves(situation: Situation) = {
    val allMoves = super.validMoves(situation)
    val capturingMoves = allMoves mapValues (_.filter(_.captures)) filterNot (_._2.isEmpty)

    if (!capturingMoves.isEmpty) capturingMoves else allMoves
  }

  // In antichess, there is no checkmate condition, and the winner is the current player if they have no legal moves
  override def winner(situation: Situation): Option[Color] = if (specialEnd(situation)) Some(situation.color) else None

  override def specialEnd(situation: Situation) = {
    // The game ends with a win when one player manages to lose all their pieces or is in stalemate
    situation.board.actorsOf(situation.color).isEmpty || situation.moves.isEmpty
  }

  // In antichess, there is no checkmate condition therefore a player may only draw either by agreement
  // or blockade (see specialDraw).
  override def insufficientWinningMaterial(situation: Situation, color: Color) = false

  // No player can win if the only remaining pieces are opposing bishops on different coloured
  // diagonals. There may be pawns that are incapable of moving and do not attack the right color
  // of square to allow the player to force their opponent to capture their bishop, also resulting in a draw
  override def insufficientWinningMaterial (board: Board) = {
    val actors = board.actors

    // Exit early if we are not in a situation with only bishops and pawns
    val bishopsAndPawns = actors.forall(act => act._2.piece.is(Bishop) || act._2.piece.is(Pawn)) &&
      actors.find(_._2.piece.is(Bishop)).isDefined

    lazy val drawnBishops = actors.values.partition(_.color == White) match {
      case (whitePieces, blackPieces) =>
        val whiteBishops = whitePieces.filter(_.piece.is(Bishop))
        val blackBishops = blackPieces.filter(_.piece.is(Bishop))
        lazy val whitePawns = whitePieces.filter(_.piece.is(Pawn))
        lazy val blackPawns = blackPieces.filter(_.piece.is(Pawn))

        // We consider the case where a player has two bishops on the same diagonal after promoting by using .distinct.
        // If after applying .distinct the size of the list is greater than one, then the player has bishops on both
        // colours
        if (whiteBishops.map(_.pos.color).toList.distinct.size != 1 ||
          blackBishops.map(_.pos.color).toList.distinct.size != 1) false
        else {
          for {
            whiteSquareColor <- whiteBishops.headOption map (_.pos.color)
            blackSquareColor <- blackBishops.headOption map (_.pos.color)
          } yield {
            whiteSquareColor != blackSquareColor && whitePawns.forall(pawnAttacksOppositeColor(_, blackSquareColor)) &&
              blackPawns.forall(pawnAttacksOppositeColor(_, whiteSquareColor))
          }
        } getOrElse false
    }

    bishopsAndPawns && drawnBishops
  }

  private def pawnAttacksOppositeColor(pawn: Actor, oppositeBishopColor: Color) = {
    val pawnImmobile = pawn.moves.isEmpty
    val cannotAttackBishop = List(pawn.pos.upLeft, pawn.pos.upRight).flatten.find(_.color == oppositeBishopColor).isEmpty

    pawnImmobile && cannotAttackBishop
  }

  // In this game variant, a king is a valid promotion
  override def isValidPromotion(promotion: Option[PromotableRole]) = promotion match {
    case None => true
    case Some(Queen | Rook | Knight | Bishop | King) => true
    case _ => false
  }

  override def roles = List(Rook, Knight, King, Bishop, Queen, Pawn)

  override def promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King)
}
