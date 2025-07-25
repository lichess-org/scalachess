package chess

import chess.variant.Variant

trait HasPosition[A]:
  extension (a: A)
    def position: Position
    inline def variant: Variant = position.variant
    inline def color: Color = position.color
    inline def history: History = position.history
