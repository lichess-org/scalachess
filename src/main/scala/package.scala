import ornicar.scalalib.ScalalibExtensions

package object chess extends ScalalibExtensions {

  val White = Color.White
  val Black = Color.Black

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  type MoveOrDrop = Either[Move, Drop]
}
