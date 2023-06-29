package chess

export ornicar.scalalib.newtypes.*
export ornicar.scalalib.zeros.*
export ornicar.scalalib.extensions.*

export Color.{ Black, White }
export Side.{ KingSide, QueenSide }

type PieceMap = Map[Square, Piece]
