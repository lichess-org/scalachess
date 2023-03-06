package chess
package format.pgn

object Dumper:

  def apply(situation: Situation, data: chess.Move, next: Situation): SanStr =
    import data.*
    import bitboard.Bitboard.*

    val base = (promotion, piece.role) match
      case _ if castles =>
        if (orig ?> dest) "O-O-O" else "O-O"

      case _ if enpassant => s"${orig.file.char}x${dest.key}"

      case (promotion, Pawn) =>
        (if (captures) s"${orig.file.char}x" else "") +
          promotion.fold(dest.key)(p => s"${dest.key}=${p.pgn}")

      case (_, role) =>
        // Check whether there is a need to disambiguate:
        //   - can a piece of same role move to/capture on the same square?
        //   - if so, disambiguate, in order or preference, by:
        //       - file
        //       - rank
        //       - both (only happens w/ at least 3 pieces of the same role)
        // We know Role ≠ Pawn, so it is fine to always pass None as promotion target
        val candidates = (situation.board.byPiece(piece) ^ orig.bb).occupiedSquares
          .filter(square =>
            piece.eyes(square, dest) && {
              situation.move(square, dest, None).isValid
            }
          )

        val disambiguation: String =
          if (candidates.isEmpty)
            ""
          else if (!candidates.exists(_ ?| orig))
            orig.file.char.toString
          else if (!candidates.exists(_ ?- orig))
            orig.rank.char.toString
          else
            orig.key

        s"${role.pgn}$disambiguation${if (captures) "x" else ""}${dest.key}"

    SanStr(s"$base${checkOrWinnerSymbol(next)}")

  def apply(data: chess.Drop, next: Situation): SanStr =
    SanStr(s"${data.toUci.uci}${checkOrWinnerSymbol(next)}")

  def apply(data: chess.Move): SanStr =
    apply(data.situationBefore, data, data.situationAfter)

  def apply(data: chess.Drop): SanStr =
    apply(data, data.situationAfter)

  private def checkOrWinnerSymbol(next: Situation): String =
    if next.winner.isDefined then "#"
    else if next.check.yes then "+"
    else ""
