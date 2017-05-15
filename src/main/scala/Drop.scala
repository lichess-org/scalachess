package chess

import format.Uci

case class Drop(
    piece: Piece,
    pos: Pos,
    before: Board,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) {

  def situationBefore = before situationOf piece.color
  def situationAfter = finalizeAfter situationOf !piece.color

  def withHistory(h: History) = copy(after = after withHistory h)

  def finalizeAfter: Board = {
    val board = after.variant.finalizeBoard(
      after updateHistory { h =>
        h.copy(
          lastMove = Some(Uci.Drop(piece.role, pos)),
          unmovedRooks = before.unmovedRooks
        )
      }, toUci, none
    )

    board updateHistory {
      _.copy(positionHashes = board.variant updatePositionHashes (board, this, board.history.positionHashes))
    }
  }

  def afterWithLastMove = after.copy(
    history = after.history.withLastMove(Uci.Drop(piece.role, pos))
  )

  def color = piece.color

  def withAfter(newBoard: Board) = copy(after = newBoard)

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci
}
