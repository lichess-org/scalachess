package chess
package variant

import chess.Role._

case object Extinction extends Variant(
  id = 11,
  key = "extinct",
  name = "Extinction",
  shortName = "extinct",
  title = "Capture any piece species to extinction to win the game.",
  standardInitialPosition = true) {

  override def specialEnd(situation: Situation) = {
    val pieces = situation.board.piecesOf(situation.color)
    Role.all exists(role => pieces forall(_._2.isNot(role)))
  }

  // the king is an ordinary piece in extinction chess, so we don't worry about it
  // promotion of your last pawn loses the game, so it is not allowed
  override def kingSafety(m: Move, filter: Piece => Boolean, kingPos: Option[Pos]): Boolean =
    m.promotion.isEmpty || (m.after.piecesOf(m.piece.color) exists(_._2.is(Pawn)))

  // castling out of, through, and into check are all allowed
  override def kingThreatened(board: Board, color: Color, to: Pos, filter: Piece => Boolean = _ => true): Boolean = false

  // you can promote to king
  override def promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King)

}
