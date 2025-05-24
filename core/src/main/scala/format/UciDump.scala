package chess
package format

import cats.syntax.all.*
import chess.variant.Variant

object UciDump:

  def apply(
      moves: Seq[pgn.SanStr],
      initialFen: Option[FullFen],
      variant: Variant,
      force960Notation: Boolean = false
  ): Either[ErrorStr, List[String]] =
    if moves.isEmpty then Nil.asRight
    else
      Position(variant, initialFen)
        .play(moves)
        .map(_.map(move(variant, force960Notation)))

  def move(variant: Variant, force960Notation: Boolean = false)(mod: MoveOrDrop): String =
    mod match
      case m: Move =>
        m.castle
          .fold(m.toUci.uci): c =>
            if force960Notation || c.king == c.kingTo || variant.chess960 || variant.fromPosition then
              c.king.key + c.rook.key
            else c.king.key + c.kingTo.key
      case d: Drop => d.toUci.uci
