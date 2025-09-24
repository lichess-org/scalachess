package chess
package format

import cats.syntax.all.*
import chess.variant.Variant

object UciDump:

  def apply(
      moves: Seq[pgn.SanStr],
      initialFen: Option[FullFen],
      variant: Variant
  ): Either[ErrorStr, List[String]] =
    if moves.isEmpty then Nil.asRight
    else
      Position(variant, initialFen)
        .play(moves, Ply.initial): step =>
          move(step.move)

  def move(mod: MoveOrDrop): String =
    mod match
      case m: Move =>
        m.castle
          .fold(m.toUci.uci): c =>
            c.king.key + c.rook.key
      case d: Drop => d.toUci.uci
