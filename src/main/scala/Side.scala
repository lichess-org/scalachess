package chess

sealed trait Side {

  def castledKingX0: Int
  def castledRookX0: Int

  def tripToRook: (Pos, Board) => List[Pos]
}

object Side {

  val all = List(KingSide, QueenSide)

  def kingRookSide(kingPos: Pos, rookPos: Pos): Option[Side] =
    if (kingPos ?- rookPos)
      Option(if (kingPos ?> rookPos) QueenSide else KingSide)
    else None
}

case object KingSide extends Side {

  val castledKingX0 = 6
  val castledRookX0 = 5

  val tripToRook: (Pos, Board) => List[Pos] = (pos, board) => pos >| board.pieces.contains
}
case object QueenSide extends Side {

  val castledKingX0 = 2
  val castledRookX0 = 3

  val tripToRook: (Pos, Board) => List[Pos] = (pos, board) => pos |< board.pieces.contains
}
