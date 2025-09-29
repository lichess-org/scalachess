package chess
package format

import cats.syntax.all.*
import chess.variant.Variant

object UciDump:

  def apply(
      moves: Seq[pgn.SanStr],
      initialFen: Option[FullFen],
      variant: Variant,
      // some API clients can't handle e1h1, so we need to send them e1g1
      legacyStandardCastling: Boolean = false
  ): Either[ErrorStr, List[String]] =
    if moves.isEmpty then Nil.asRight
    else
      Position(variant, initialFen)
        .play(moves, Ply.initial): step =>
          move(step.move, variant, legacyStandardCastling)

  def move(mod: MoveOrDrop, variant: Variant, legacyStandardCastling: Boolean = false): String =
    mod match
      case m: Move =>
        m.castle
          .fold(m.toUci.uci): c =>
            if legacyStandardCastling && variant.standard
            then c.king.key + c.kingTo.key
            else c.king.key + c.rook.key
      case d: Drop => d.toUci.uci
