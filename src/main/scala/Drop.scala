package chess

import cats.syntax.option.none

import chess.format.Uci

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) {

  def before = situationBefore.board

  def situationAfter = Situation(finalizeAfter, !piece.color)

  def withHistory(h: History) = copy(after = after withHistory h)

  def finalizeAfter: Board = {
    val board = after.variant.finalizeBoard(
      after updateHistory { h =>
        h.copy(
          lastMove = Option(Uci.Drop(piece.role, pos)),
          unmovedRooks = before.unmovedRooks,
          halfMoveClock = if (piece is Pawn) 0 else h.halfMoveClock + 1
        )
      },
      toUci,
      none
    )

    board updateHistory { h =>
      val basePositionHashes =
        if (h.positionHashes.isEmpty) Hash(situationBefore) else board.history.positionHashes
      h.copy(positionHashes = Hash(Situation(board, !piece.color)) ++ basePositionHashes)
    }
  }

  def afterWithLastMove =
    after.variant.finalizeBoard(
      after.copy(history = after.history.withLastMove(toUci)),
      toUci,
      none
    )

  def color = piece.color

  def withAfter(newBoard: Board) = copy(after = newBoard)

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci
}
