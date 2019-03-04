package chess
package variant

import scala.collection.breakOut

case object Antichess extends Variant(
  id = 6,
  key = "antichess",
  name = "Antichess",
  shortName = "Anti",
  title = "Lose all your pieces (or get stalemated) to win the game.",
  standardInitialPosition = true
) {

  def pieces = Standard.pieces

  // In antichess, it is not permitted to castle
  override val castles = Castles.none
  override val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1"

  // In antichess, the king can't be put into check so we always return false
  override def kingThreatened(board: Board, color: Color, to: Pos, filter: Piece => Boolean = _ => true) = false

  // In this variant, a player must capture if a capturing move is available
  override def validMoves(situation: Situation) = {
    val allMoves = super.validMoves(situation)
    val capturingMoves = allMoves mapValues (_.filter(_.captures)) filterNot (_._2.isEmpty)

    if (!capturingMoves.isEmpty) capturingMoves else allMoves
  }

  override def valid(board: Board, strict: Boolean) =
    board.pieces.size >= 2 && board.pieces.size <= 32

  // In antichess, there is no checkmate condition, and the winner is the current player if they have no legal moves
  override def winner(situation: Situation): Option[Color] = if (specialEnd(situation)) Some(situation.color) else None

  override def specialEnd(situation: Situation) = {
    // The game ends with a win when one player manages to lose all their pieces or is in stalemate
    situation.board.piecesOf(situation.color).isEmpty || situation.moves.isEmpty
  }

  // In antichess, there is no checkmate condition therefore a player may only draw either by agreement
  // , blockade or stalemate - a player always has sufficient material to win otherwise
  override def insufficientWinningMaterial(situation: Situation) = false

  // No player can win if the only remaining pieces are opposing bishops on different coloured
  // diagonals. There may be pawns that are incapable of moving and do not attack the right color
  // of square to allow the player to force their opponent to capture their bishop, also resulting in a draw
  override def insufficientWinningMaterial(board: Board) = {
    val pieces = board.pieces

    // Exit early if we are not in a situation with only bishops and pawns
    val bishopsAndPawns = pieces.forall(p => p._2.is(Bishop) || p._2.is(Pawn)) &&
      pieces.find(_._2.is(Bishop)).isDefined

    lazy val drawnBishops = board.pieces.partition(_._2.color == White) match {
      case (whitePieces, blackPieces) =>
        val whiteBishops = whitePieces.filter(_._2.is(Bishop))
        val blackBishops = blackPieces.filter(_._2.is(Bishop))
        lazy val whitePawns = board.piecesOf(White).filter(_._2.is(Pawn)).map { case (pos, piece) => Actor(piece, pos, board) }
        lazy val blackPawns = board.piecesOf(Black).filter(_._2.is(Pawn)).map { case (pos, piece) => Actor(piece, pos, board) }

        // We consider the case where a player has two bishops on the same diagonal after promoting by using .distinct.
        // If after applying .distinct the size of the list is greater than one, then the player has bishops on both colours
        if (whiteBishops.map(_._1.color)(breakOut).distinct.size != 1 ||
          blackBishops.map(_._1.color)(breakOut).distinct.size != 1) false
        else {
          for {
            whiteSquareColor <- whiteBishops.headOption map (_._1.color)
            blackSquareColor <- blackBishops.headOption map (_._1.color)
          } yield {
            whiteSquareColor != blackSquareColor && whitePawns.forall(pawnNotAttackable(_, blackSquareColor, board)) &&
              blackPawns.forall(pawnNotAttackable(_, whiteSquareColor, board))
          }
        } getOrElse false
    }

    bishopsAndPawns && drawnBishops
  }

  private def pawnNotAttackable(pawn: Actor, oppositeBishopColor: Color, board: Board) = {
    // The pawn cannot attack a bishop or be attacked by a bishop
    val cannotAttackBishop = Actor.pawnAttacks(pawn.pos, pawn.piece.color).find(_.color == oppositeBishopColor).isEmpty

    InsufficientMatingMaterial.pawnBlockedByPawn(pawn, board) && cannotAttackBishop
  }

  // In this game variant, a king is a valid promotion
  override def isValidPromotion(promotion: Option[PromotableRole]) = promotion match {
    case None => true
    case Some(Queen | Rook | Knight | Bishop | King) => true
    case _ => false
  }

  override val roles = List(Rook, Knight, King, Bishop, Queen, Pawn)

  override val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King)
}
