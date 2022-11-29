package chess

object Chess extends ornicar.scalalib.Zeros

export Chess.{ *, given }

export ornicar.scalalib.OrnicarBooleanWrapper
export ornicar.scalalib.ScalalibExtensions.*
export ornicar.scalalib.newtypes.*

export Color.{ Black, White }

type Direction  = Pos => Option[Pos]
type Directions = List[Direction]

type PieceMap = Map[Pos, Piece]

type PositionHash = Array[Byte]

type MoveOrDrop = Either[Move, Drop]
