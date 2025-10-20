package chess
package format

import cats.syntax.all.*
import chess.variant.{ Chess960, Variant }

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
          move(step.move, legacyStandardCastling && variant.standard)

  def move(mod: MoveOrDrop, legacyStandardCastling: Boolean = false): String =
    mod match
      case m: Move =>
        m.castle
          .fold(m.toUci.uci): c =>
            if legacyStandardCastling
            then c.king.key + c.kingTo.key
            else c.king.key + c.rook.key
      case d: Drop => d.toUci.uci

  // Keys to highlight to show the last move made on the board.
  // Does not render as UCI.
  def lastMove(uci: Uci, variant: Variant): String = uci match
    case d: Uci.Drop => d.square.key * 2
    case m: Uci.Move =>
      if variant == Chess960 then m.keys
      else
        m.keys match
          case "e1h1" => "e1g1"
          case "e8h8" => "e8g8"
          case "e1a1" => "e1c1"
          case "e8a8" => "e8c8"
          case k => k
