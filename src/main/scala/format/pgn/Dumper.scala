package chess
package format.pgn

object Dumper {

  def apply(situation: Situation, data: chess.Move, next: Situation): String = {
    import data._
    ((promotion, piece.role) match {
      case _ if castles   ⇒ if (orig ?> dest) "O-O-O" else "O-O"
      case _ if enpassant ⇒ orig.file + 'x' + dest.key
      case (promotion, Pawn) ⇒
        captures.fold(orig.file + "x", "") +
          promotion.fold(dest.key)(p ⇒ dest.key + "=" + p.pgn)
      case (_, role) ⇒ role.pgn + {
        val candidates = situation.board.pieces collect {
          case (cpos, cpiece) if cpiece == piece && cpos != orig && cpiece.eyes(cpos, dest) ⇒ cpos
        }
        if (candidates.isEmpty) ""
        else if (candidates exists (_ ?| orig)) orig.file + orig.rank else orig.file
      } + captures.fold("x", "") + dest.key
    }) + (if (next.check) if (next.checkMate) "#" else "+" else "")
  }

  def apply(data: chess.Move): String = apply(
    data.before situationOf data.color,
    data,
    data.after situationOf !data.color)
}
