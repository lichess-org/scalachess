package chess
package format

// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1 b kq - 6 20
opaque type Fen = String
object Fen extends OpaqueString[Fen]:
  extension (a: Fen)
    def halfMove: Option[Int] = a.value.split(' ').lift(4).flatMap(_.toIntOption)
    def fullMove: Option[Int] = a.value.split(' ').lift(5).flatMap(_.toIntOption)

    def color: Option[Color] =
      a.value.split(' ').lift(1) flatMap (_.headOption) flatMap Color.apply

    def ply: Option[Int] =
      fullMove map { _ * 2 - (if (color.exists(_.white)) 2 else 1) }

    def initial = a == Forsyth.initial

    def board: BoardFen     = a.takeWhile(_ != ' ')
    def opening: OpeningFen = OpeningFen fromFen a

  def clean(source: String): Fen = Fen(source.replace("_", " ").trim)

// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1 b kq -
opaque type OpeningFen = String
object OpeningFen extends OpaqueString[OpeningFen]:
  extension (a: OpeningFen) def board: BoardFen = a.value.takeWhile(_ != ' ')
  def fromFen(fen: Fen): OpeningFen =
    fen.value.split(' ').take(4) match
      case Array(board, turn, castle, ep) =>
        OpeningFen(s"${BoardFen(board).removePockets} $turn $castle $ep")
      case _ => fen into OpeningFen

// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1 b
opaque type BoardAndColorFen = String
object BoardAndColorFen extends OpaqueString[BoardAndColorFen]

// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1
opaque type BoardFen = String
object BoardFen extends OpaqueString[BoardFen]:
  extension (a: BoardFen)
    def andColor(c: Color) = BoardAndColorFen(s"$a ${c.letter}")
    def removePockets: BoardFen =
      if (a.contains('[')) a.takeWhile('[' !=)
      else if (a.count('/' == _) == 8) a.split('/').take(8).mkString("/")
      else a
