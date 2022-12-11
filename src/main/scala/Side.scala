package chess

enum Side:
  case KingSide, QueenSide

  inline def fold[A](k: => A, q: => A): A = if isKingSide then k else q

  lazy val castledKingFile: File = fold(File.G, File.C)
  lazy val castledRookFile: File = fold(File.F, File.D)

  lazy val tripToRook: (Pos, Board) => List[Pos] = fold(
    (pos, board) => pos >| board.pieces.contains,
    (pos, board) => pos |< board.pieces.contains
  )

  private lazy val isKingSide = this == Side.KingSide

object Side:

  val all = List(KingSide, QueenSide)

  def kingRookSide(kingPos: Pos, rookPos: Pos): Option[Side] =
    if (kingPos ?- rookPos)
      Option(if (kingPos ?> rookPos) QueenSide else KingSide)
    else None
