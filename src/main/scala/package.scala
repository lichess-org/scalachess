package chess

export ornicar.scalalib.newtypes.*
export ornicar.scalalib.zeros.*
export ornicar.scalalib.extensions.*

export Color.{ Black, White }

type Direction  = Pos => Option[Pos]
type Directions = List[Direction]

type PieceMap = Map[Pos, Piece]

type MoveOrDrop = Either[Move, Drop]
