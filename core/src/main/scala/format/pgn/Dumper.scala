package chess
package format.pgn

object Dumper:

  def apply(position: Position, data: chess.Move, next: Position): SanStr =
    import data.*

    val base = (promotion, piece.role) match
      case _ if castles =>
        if orig ?> dest then "O-O-O" else "O-O"

      case _ if enpassant => s"${orig.file.char}x${dest.key}"

      case (promotion, Pawn) =>
        (if captures then s"${orig.file.char}x" else "") +
          promotion.fold(dest.key)(p => s"${dest.key}=${p.pgn}")

      case (_, role) =>
        // Check whether there is a need to disambiguate:
        //   - can a piece of same role move to/capture on the same square?
        //   - if so, disambiguate, in order or preference, by:
        //       - file
        //       - rank
        //       - both (only happens w/ at least 3 pieces of the same role)
        // We know Role ≠ Pawn, so it is fine to always pass None as promotion target
        val candidates = (position.byPiece(piece) ^ orig.bl)
          .filter(square =>
            piece.eyes(square, dest, position.occupied) && {
              position.move(square, dest, None).isRight
            }
          )

        val disambiguation: String =
          if candidates.isEmpty then ""
          else if !candidates.exists(_.onSameFile(orig)) then orig.file.char.toString
          else if !candidates.exists(_.onSameRank(orig)) then orig.rank.char.toString
          else orig.key

        val x = if captures then "x" else ""
        s"${role.pgn}$disambiguation$x${dest.key}"

    SanStr(s"$base${checkOrWinnerSymbol(next)}")

  def apply(data: chess.Drop, next: Position): SanStr =
    SanStr(s"${data.toUci.uci}${checkOrWinnerSymbol(next)}")

  def apply(data: chess.Move): SanStr =
    apply(data.boardBefore, data, data.boardAfter)

  def apply(data: chess.Drop): SanStr =
    apply(data, data.boardAfter)

  private def checkOrWinnerSymbol(next: Position): String =
    if next.winner.isDefined then "#"
    else if next.check.yes then "+"
    else ""
