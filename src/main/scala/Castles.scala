package chess

import bitboard.OpaqueBitboard
import bitboard.Bitboard
import bitboard.Bitboard.given

import Pos.*

opaque type Castles = Long
object Castles extends OpaqueBitboard[Castles]:

  import cats.syntax.all.*

  extension (c: Castles)

    inline def can(inline color: Color) = Castles.Can(c, color)

    def whiteKingSide: Boolean  = (c & H1.bitboard).nonEmpty
    def whiteQueenSide: Boolean = (c & A1.bitboard).nonEmpty
    def blackKingSide: Boolean  = (c & H8.bitboard).nonEmpty
    def blackQueenSide: Boolean = (c & A8.bitboard).nonEmpty

    def without(color: Color): Castles =
      c & Bitboard.rank(color.lastRank)

    def without(color: Color, side: Side): Castles =
      (color, side) match
        case (White, KingSide)  => c & ~H1.bitboard
        case (White, QueenSide) => c & ~A1.bitboard
        case (Black, KingSide)  => c & ~H8.bitboard
        case (Black, QueenSide) => c & ~A8.bitboard

    def add(color: Color, side: Side): Castles =
      (color, side) match
        case (White, KingSide)  => c | H1.bitboard
        case (White, QueenSide) => c | A1.bitboard
        case (Black, KingSide)  => c | H8.bitboard
        case (Black, QueenSide) => c | A8.bitboard

    def update(color: Color, kingSide: Boolean, queenSide: Boolean): Castles = color match
      case White => c.without(color) | kingSide.whiteKing | queenSide.whiteQueen
      case Black => c.without(color) | kingSide.blackKing | queenSide.blackQueen

    def toFenString: String = {
      (if (whiteKingSide) "K" else "") +
        (if (whiteQueenSide) "Q" else "") +
        (if (blackKingSide) "k" else "") +
        (if (blackQueenSide) "q" else "")
    } match
      case "" => "-"
      case n  => n

    def toSeq: Array[Boolean] = Array(whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide)

  extension (b: Boolean)
    def whiteKing: Castles  = if (b) H1.bitboard else empty
    def whiteQueen: Castles = if (b) A1.bitboard else empty
    def blackKing: Castles  = if (b) H8.bitboard else empty
    def blackQueen: Castles = if (b) A8.bitboard else empty

  def apply(
      whiteKingSide: Boolean,
      whiteQueenSide: Boolean,
      blackKingSide: Boolean,
      blackQueenSide: Boolean
  ): Castles =
    val whiteKing  = whiteKingSide.whiteKing
    val whiteQueen = whiteQueenSide.whiteQueen
    val blackKing  = blackKingSide.blackKing
    val blackQueen = blackQueenSide.blackQueen
    whiteKing | whiteQueen | blackKing | blackQueen

  def apply(str: String): Castles = str match
    case "-" => empty
    case _ =>
      str.toList
        .traverse(charToSquare)
        .map(_.foldRight(empty)((s, b) => s.bitboard | b))
        .getOrElse(empty)

  private def charToSquare: (c: Char) => Option[Pos] =
    case 'k' => Some(H8)
    case 'q' => Some(A1)
    case 'K' => Some(H1)
    case 'Q' => Some(A1)
    case _   => None

  val all: Castles  = CORNERS
  val init: Castles = all
  val none: Castles = empty

  final class Can(castles: Castles, color: Color):
    def on(side: Side): Boolean =
      (color, side) match
        case (White, KingSide)  => castles.whiteKingSide
        case (White, QueenSide) => castles.whiteQueenSide
        case (Black, KingSide)  => castles.blackKingSide
        case (Black, QueenSide) => castles.blackQueenSide
    def any = on(KingSide) || on(QueenSide)

opaque type UnmovedRooks = Long
object UnmovedRooks extends OpaqueBitboard[UnmovedRooks]:
  // for lila testing only
  val default: UnmovedRooks = UnmovedRooks(Bitboard.rank(Rank.First) | Bitboard.rank(Rank.Eighth))
  val corners: UnmovedRooks = CORNERS
  val none: UnmovedRooks    = empty

  def apply(b: Bitboard): UnmovedRooks   = b.value
  def apply(set: Set[Pos]): UnmovedRooks = set.foldLeft(empty)((b, p) => b | p.bitboard)

  extension (ur: UnmovedRooks)
    def toList: List[Pos]        = ur.occupiedSquares
    def apply(pos: Pos): Boolean = (ur & pos.bitboard).nonEmpty

    def without(color: Color): UnmovedRooks =
      ur & Bitboard.rank(color.lastRank)

    // Try to guess the side of the rook at postion `pos`
    // If the position is not a ummovedRook return None
    // If the position is a ummovedRook but there is no other rook on the
    // same rank return Some(None) (because we cannot guess)
    // If there are two rooks on the same rank, return the side of the rook
    def side(pos: Pos): Option[Option[Side]] =
      val bitboard = pos.bitboard
      if (ur & bitboard).isEmpty then None
      else
        (ur & ~bitboard).first match
          case Some(otherRook) =>
            if (otherRook.file > pos.file) then Some(Some(QueenSide))
            else Some(Some(KingSide))
          case None => Some(None)
