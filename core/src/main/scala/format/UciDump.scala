package chess
package format

import cats.syntax.all.*
import chess.format.pgn.Parser
import chess.variant.Variant

object UciDump:

  // a2a4, b8c6
  def apply(force960Notation: Boolean)(replay: Replay): List[String] =
    replay.chronoMoves.map(move(replay.setup.variant, force960Notation))

  def apply(
      moves: Seq[pgn.SanStr],
      initialFen: Option[FullFen],
      variant: Variant,
      force960Notation: Boolean = false
  ): Either[ErrorStr, List[String]] =
    if moves.isEmpty then Nil.asRight
    else
      Parser
        .moves(moves)
        .flatMap: sans =>
          Position(variant, initialFen).play(sans.value)
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
