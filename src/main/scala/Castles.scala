package chess

import cats.syntax.all.*

import bitboard.OpaqueBitboard
import bitboard.Bitboard
import bitboard.Bitboard.given

import Pos.*

opaque type Castles = Long
object Castles extends OpaqueBitboard[Castles]:

  extension (c: Castles)

    inline def can(inline color: Color) = Castles.Can(c, color)

    inline def apply(color: Color, side: Side): Pos =
      (color, side) match
        case (White, KingSide)  => H1
        case (White, QueenSide) => A1
        case (Black, KingSide)  => H8
        case (Black, QueenSide) => A8

    inline def apply(p: (Color, Side)): Pos = apply(p._1, p._2)

    def whiteKingSide: Boolean  = (c & H1.bb).nonEmpty
    def whiteQueenSide: Boolean = (c & A1.bb).nonEmpty
    def blackKingSide: Boolean  = (c & H8.bb).nonEmpty
    def blackQueenSide: Boolean = (c & A8.bb).nonEmpty

    def without(color: Color): Castles =
      c & Bitboard.rank(color.lastRank)

    def without(color: Color, side: Side): Castles =
      c & ~apply(color, side).bb

    def add(color: Color, side: Side): Castles =
      c.addPos(apply(color, side))

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
    def whiteKing: Castles  = if (b) H1.bb else empty
    def whiteQueen: Castles = if (b) A1.bb else empty
    def blackKing: Castles  = if (b) H8.bb else empty
    def blackQueen: Castles = if (b) A8.bb else empty

  def apply(
      whiteKingSide: Boolean,
      whiteQueenSide: Boolean,
      blackKingSide: Boolean,
      blackQueenSide: Boolean
  ): Castles =
    whiteKingSide.whiteKing |
      whiteQueenSide.whiteQueen |
      blackKingSide.blackKing |
      blackQueenSide.blackQueen

  def apply(str: String): Castles = str match
    case "-" => empty
    case _ =>
      str.toList
        .traverse(charToSquare)
        .map(_.foldRight(empty)((p, b) => b.addPos(p)))
        .getOrElse(empty)

  private def charToSquare: (c: Char) => Option[Pos] =
    case 'k' => Some(H8)
    case 'q' => Some(A8)
    case 'K' => Some(H1)
    case 'Q' => Some(A1)
    case _   => None

  val all: Castles  = CORNERS
  val init: Castles = all
  val none: Castles = empty

  final class Can(castles: Castles, color: Color):
    def on(side: Side): Boolean =
      (castles & castles(color, side).bb).nonEmpty

    def any = on(KingSide) || on(QueenSide)
