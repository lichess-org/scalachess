package chess

import cats.syntax.option.none
import chess.format.Uci

case class Drop(
    piece: Piece,
    square: Square,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics.empty
):

  inline def before = situationBefore.board

  lazy val situationAfter = Situation(finalizeAfter, !piece.color)

  lazy val san = format.pgn.Dumper(this)

  inline def withHistory(inline h: History) = copy(after = after withHistory h)

  lazy val finalizeAfter: Board =
    val board = after.variant.finalizeBoard(
      after updateHistory { h =>
        h.copy(
          lastMove = Option(Uci.Drop(piece.role, square)),
          unmovedRooks = before.unmovedRooks,
          halfMoveClock = if piece.is(Pawn) then HalfMoveClock.initial else h.halfMoveClock + 1
        )
      },
      toUci,
      none
    )

    board updateHistory { h =>
      val basePositionHashes =
        if h.positionHashes.value.isEmpty then Hash(situationBefore) else h.positionHashes
      h.copy(positionHashes = Hash(Situation(board, !piece.color)).combine(basePositionHashes))
    }

  def afterWithLastMove =
    after.variant.finalizeBoard(
      after.copy(history = after.history.withLastMove(toUci)),
      toUci,
      none
    )

  inline def color = piece.color

  inline def withAfter(newBoard: Board) = copy(after = newBoard)

  inline def withMetrics(m: MoveMetrics) = copy(metrics = m)

  inline def toUci = Uci.Drop(piece.role, square)

  override def toString = toUci.uci
