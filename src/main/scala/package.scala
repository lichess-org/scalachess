package chess

import cats.data.Validated

export ornicar.scalalib.ScalalibExtensions.*
export ornicar.scalalib.OrnicarBooleanWrapper

extension [E, A](validated: Validated[E, A])
  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = validated.andThen(f)

export Color.{ Black, White }

type Direction  = Pos => Option[Pos]
type Directions = List[Direction]

type PieceMap = Map[Pos, Piece]

type PositionHash = Array[Byte]

type MoveOrDrop = Either[Move, Drop]
