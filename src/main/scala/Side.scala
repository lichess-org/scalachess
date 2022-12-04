package chess

enum Side:
  case KingSide, QueenSide

  inline def fold[A](k: => A, q: => A): A = if (king) k else q

  lazy val castledKingFile: File = fold(File.G, File.C)
  lazy val castledRookFile: File = fold(File.F, File.D)

  def tripToRook: (Pos, Board) => List[Pos] = (pos, board) =>
    this match
      case Side.KingSide => pos >| board.pieces.contains
      case Side.QueenSide => pos |< board.pieces.contains

  private lazy val king = this == Side.KingSide

object Side:

  val all = List(KingSide, QueenSide)

  def kingRookSide(kingPos: Pos, rookPos: Pos): Option[Side] =
    if (kingPos ?- rookPos)
      Option(if (kingPos ?> rookPos) QueenSide else KingSide)
    else None
