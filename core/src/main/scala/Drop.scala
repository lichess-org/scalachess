package chess

import cats.syntax.option.none
import chess.format.Uci
import chess.format.pgn.SanStr

case class Drop(
    piece: Piece,
    square: Square,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics.empty
):

  inline def before    = situationBefore.board
  def situationAfter   = Situation(finalizeAfter, !piece.color)
  lazy val san: SanStr = format.pgn.Dumper(this)

  lazy val finalizeAfter: Board =
    val board = after.variant.finalizeBoard(
      after.updateHistory { h =>
        h.copy(
          lastMove = Option(Uci.Drop(piece.role, square)),
          unmovedRooks = before.unmovedRooks,
          halfMoveClock = if piece.is(Pawn) then HalfMoveClock.initial else h.halfMoveClock + 1
        )
      },
      toUci,
      none
    )

    board.updateHistory { h =>
      val basePositionHashes =
        if h.positionHashes.value.isEmpty then PositionHash(Hash(situationBefore)) else h.positionHashes
      h.copy(positionHashes = PositionHash(Hash(Situation(board, !piece.color))).combine(basePositionHashes))
    }

  inline def withHistory(inline h: History) = copy(after = after.withHistory(h))

  def afterWithLastMove: Board =
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
