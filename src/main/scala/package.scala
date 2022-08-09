import ornicar.scalalib

package object chess extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val Red = Color.Red
  val Black = Color.Black

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  type MoveOrDrop = Either[Move, Drop]
}
