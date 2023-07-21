package chess
package format

object Fen extends FenReader with FenWriter:
  export format.{ BoardFen as Board, EpdFen as Epd, OpeningFen as Opening, SimpleFen as Simple }
  export EpdFen.initial

// https://www.chessprogramming.org/Extended_Position_Description
// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1 b kq - 6 20
// rnbqkbnr/ppp1pppp/8/1B1p4/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2 +1+0 (3check)
// r1bqkbnr/pppp1Qpp/2n5/4p3/4P3/8/PPPP1PPP/RNB1KBNR b KQkq - 2+3 0 3 (winboards 3check)
opaque type EpdFen = String
object EpdFen extends OpaqueString[EpdFen]:
  extension (a: EpdFen)
    def color: Option[Color]      = SimpleFen.color(a)
    def colorOrWhite: Color       = SimpleFen.colorOrWhite(a)
    def castling: String          = SimpleFen castling a
    def enpassant: Option[Square] = SimpleFen enpassant a

    def isInitial: Boolean = a == initial

    def simple: SimpleFen   = SimpleFen fromEpd a
    def opening: OpeningFen = SimpleFen opening a
    def board: BoardFen     = SimpleFen board a

  val initial: EpdFen               = EpdFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  def clean(source: String): EpdFen = EpdFen(source.replace("_", " ").trim)

// a.k.a. just FEN.
// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1 b kq -
opaque type SimpleFen = String
object SimpleFen extends OpaqueString[SimpleFen]:
  extension (a: SimpleFen)
    def color: Option[Color]      = a.split(' ').lift(1).flatMap(_.headOption).flatMap(Color.apply)
    def colorOrWhite: Color       = color | Color.White
    def castling: String          = a.split(' ').lift(2) | "-"
    def enpassant: Option[Square] = a.split(' ').lift(3).flatMap(Square.fromKey(_))
    def opening: OpeningFen       = OpeningFen.fromSimple(a)
    def board: BoardFen           = a.takeWhile(_ != ' ')
  def fromEpd(fen: EpdFen): OpeningFen =
    fen.value.split(' ').take(4) match
      case Array(board, turn, castle, ep) => SimpleFen(s"$board $turn $castle $ep")
      case _                              => fen into SimpleFen

// Like SimpleFen, but for standard chess, without ZH pockets
opaque type OpeningFen = String
object OpeningFen extends OpaqueString[OpeningFen]:
  extension (a: OpeningFen) def board: BoardFen = a.value.takeWhile(_ != ' ')
  def fromEpd(fen: EpdFen): OpeningFen          = fromSimple(EpdFen simple fen)
  def fromSimple(fen: SimpleFen): OpeningFen =
    fen.value.split(' ').take(4) match
      case Array(board, turn, castle, ep) =>
        OpeningFen(s"${BoardFen(board).removePockets} $turn $castle $ep")
      case _ => fen into OpeningFen
  val initial: OpeningFen = EpdFen.initial.opening

// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1 b
opaque type BoardAndColorFen = String
object BoardAndColorFen extends OpaqueString[BoardAndColorFen]

// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1
opaque type BoardFen = String
object BoardFen extends OpaqueString[BoardFen]:
  extension (a: BoardFen)
    def andColor(c: Color) = BoardAndColorFen(s"$a ${c.letter}")
    def removePockets: BoardFen =
      if a.contains('[') then a.takeWhile('[' !=)
      else if a.count('/' == _) == 8 then a.split('/').take(8).mkString("/")
      else a
