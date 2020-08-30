package chess

sealed trait Side {

  def castledKingFile: File
  def castledRookFile: File

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

  val castledKingFile = File.G
  val castledRookFile = File.F

  val tripToRook: (Pos, Board) => List[Pos] = (pos, board) => pos >| board.pieces.contains
}
case object QueenSide extends Side {

  val castledKingFile = File.C
  val castledRookFile = File.D

  val tripToRook: (Pos, Board) => List[Pos] = (pos, board) => pos |< board.pieces.contains
}
