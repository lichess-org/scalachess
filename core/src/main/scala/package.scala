package chess

export scalalib.newtypes.*
export scalalib.zeros.*
export scalalib.extensions.*

export Color.{ Black, White }
export Side.{ KingSide, QueenSide }

type PieceMap = Map[Square, Piece]
