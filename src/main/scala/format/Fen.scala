package chess
package format

object Fen extends FenReader with FenWriter:
  export format.{ BoardFen as Board, EpdFen as Epd, SimpleFen as Simple, StandardFen as Standard }
  export EpdFen.initial

// https://www.chessprogramming.org/Extended_Position_Description
// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1 b kq - 6 20
// rnbqkbnr/ppp1pppp/8/1B1p4/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2 +1+0 (3check)
// r1bqkbnr/pppp1Qpp/2n5/4p3/4P3/8/PPPP1PPP/RNB1KBNR b KQkq - 2+3 0 3 (winboards 3check)
opaque type EpdFen = String
object EpdFen extends OpaqueString[EpdFen]:

  extension (a: EpdFen)
    def parts: (String, Option[Color], String, Option[Square]) =
      val parts = a.split(' ')
      (
        a.takeWhile(_ != ' '),
        getColor(parts),
        parts.lift(2).getOrElse("-"),
        parts.lift(3).flatMap(Square.fromKey(_))
      )
    def colorOrWhite: Color = SimpleFen.colorOrWhite(a)

    def isInitial: Boolean = a == initial

    def simple: SimpleFen    = SimpleFen `fromEpd` a
    def opening: StandardFen = SimpleFen `opening` a
    def board: BoardFen      = SimpleFen `board` a

  val initial: EpdFen               = EpdFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  def clean(source: String): EpdFen = EpdFen(source.replace("_", " ").trim)
  def getColor(arr: Array[String]): Option[Color] =
    if arr.length < 2 then None
    else if arr(1).size != 1 then None
    else Color(arr(1).head)

// a.k.a. just FEN.
// r3k2r/p3n1pp/2q2p2/4n1B1/5Q2/5P2/PP3P1P/R4RK1 b kq -
opaque type SimpleFen = String
object SimpleFen extends OpaqueString[SimpleFen]:
  extension (a: SimpleFen)
    def color: Option[Color]      = a.split(' ').lift(1).flatMap(_.headOption).flatMap(Color.apply)
    def colorOrWhite: Color       = color | Color.White
    def castling: String          = a.split(' ').lift(2) | "-"
    def enpassant: Option[Square] = a.split(' ').lift(3).flatMap(Square.fromKey(_))
    def opening: StandardFen      = StandardFen.fromSimple(a)
    def board: BoardFen           = a.takeWhile(_ != ' ')
  def fromEpd(fen: EpdFen): StandardFen =
    fen.value.split(' ').take(4) match
      case Array(board, turn, castle, ep) => SimpleFen(s"$board $turn $castle $ep")
      case _                              => fen into SimpleFen

// Like SimpleFen, but for standard chess, without ZH pockets
opaque type StandardFen = String
object StandardFen extends OpaqueString[StandardFen]:
  extension (a: StandardFen) def board: BoardFen = a.value.takeWhile(_ != ' ')
  def fromEpd(fen: EpdFen): StandardFen          = fromSimple(EpdFen `simple` fen)
  def fromSimple(fen: SimpleFen): StandardFen =
    fen.value.split(' ').take(4) match
      case Array(board, turn, castle, ep) =>
        StandardFen(s"${BoardFen(board).removePockets} $turn $castle $ep")
      case _ => fen into StandardFen
  val initial: StandardFen = EpdFen.initial.opening

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

// 6k17p6r18887P5BRKb
// Silly compression used to store cloud evals
opaque type SmallFen = String
object SmallFen extends OpaqueString[SmallFen]:
  import chess.variant.Variant
  extension (e: SmallFen)
    // The SmallFen notation was never intended to be read back into FEN.
    // This is inefficient and fragile. Only used by the cloud eval export for database.lichess.org.
    // Do not use it for anything else.
    // Untested with variants.
    def garbageInefficientReadBackIntoFen: SimpleFen =
      val (board, error)     = Fen.makeBoardWithCrazyPromoted(e, chess.variant.Standard)
      val boardStr           = Fen.writeBoard(board)
      val boardSize          = boardStr.value.count(_ != '/')
      val isBlackTurn        = e.lift(boardSize).exists(_ == 'b') && !e.lift(boardSize + 1).exists(_.isDigit)
      val turnSize           = if isBlackTurn then 1 else 0
      val castlingStr        = e.drop(boardSize + turnSize).takeWhile(Set('K', 'Q', 'k', 'q').contains)
      val enpassantStr       = e.drop(boardSize + turnSize + castlingStr.size)
      val colorChar          = if isBlackTurn then 'b' else 'w'
      def orNot(str: String) = if str.isEmpty then "-" else str
      SimpleFen:
        s"$boardStr $colorChar ${orNot(castlingStr)} ${orNot(enpassantStr)}"

  def make(variant: Variant, fen: Fen.Simple): SmallFen =
    val base = fen.value.split(' ').take(4).mkString("").filter { c =>
      c != '/' && c != '-' && c != 'w'
    }
    if variant == chess.variant.ThreeCheck
    then fen.value.split(' ').lift(6).foldLeft(base)(_ + _)
    else base

  def validate(variant: Variant, fen: Fen.Epd): Option[SmallFen] =
    Fen.read(variant, fen).exists(_ `playable` false) option make(variant, fen.simple)
