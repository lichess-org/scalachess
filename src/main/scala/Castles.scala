package chess

import cats.syntax.all.*

import bitboard.OpaqueBitboard
import bitboard.Bitboard
import Pos.*

opaque type Castles = Long
object Castles extends OpaqueBitboard[Castles]:

  extension (c: Castles)

    inline def can(inline color: Color) = Castles.Can(c, color)

    def whiteKingSide: Boolean  = c.contains(H1)
    def whiteQueenSide: Boolean = c.contains(A1)
    def blackKingSide: Boolean  = c.contains(H8)
    def blackQueenSide: Boolean = c.contains(A8)

    def without(color: Color): Castles =
      c & Bitboard.rank(color.lastRank)

    def without(color: Color, side: Side): Castles =
      c & ~color(side).bb

    def add(color: Color, side: Side): Castles =
      c.addPos(color(side))

    def update(color: Color, kingSide: Boolean, queenSide: Boolean): Castles =
      c.without(color) | kingSide.at(color(KingSide)) | queenSide.at(color(QueenSide))

    def toFenString: String = {
      (if (whiteKingSide) "K" else "") +
        (if (whiteQueenSide) "Q" else "") +
        (if (blackKingSide) "k" else "") +
        (if (blackQueenSide) "q" else "")
    } match
      case "" => "-"
      case n  => n

    def toSeq: Array[Boolean] = Array(whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide)

  extension (b: Boolean) inline def at(pos: Pos) = if b then pos.bb else empty

  extension (color: Color)
    inline def apply(side: Side): Pos =
      (color, side) match
        case (White, KingSide)  => H1
        case (White, QueenSide) => A1
        case (Black, KingSide)  => H8
        case (Black, QueenSide) => A8

    inline def kingSide: Pos  = apply(KingSide)
    inline def queenSide: Pos = apply(QueenSide)

  def apply(
      whiteKingSide: Boolean,
      whiteQueenSide: Boolean,
      blackKingSide: Boolean,
      blackQueenSide: Boolean
  ): Castles =
    whiteKingSide.at(White.kingSide) |
      whiteQueenSide.at(White.queenSide) |
      blackKingSide.at(Black.kingSide) |
      blackQueenSide.at(Black.queenSide)

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
      castles.contains(color(side))

    def any = on(KingSide) || on(QueenSide)
