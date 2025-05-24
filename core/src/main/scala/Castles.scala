package chess

import scala.annotation.targetName

import Square.*

opaque type Castles = Long
object Castles:

  extension (c: Castles)

    inline def can(inline color: Color): Boolean           = Bitboard.rank(color.backRank).intersects(c)
    inline def can(inline color: Color, inline side: Side) = c.contains(color.at(side))

    def isEmpty = c == 0L

    def whiteKingSide: Boolean  = c.contains(H1)
    def whiteQueenSide: Boolean = c.contains(A1)
    def blackKingSide: Boolean  = c.contains(H8)
    def blackQueenSide: Boolean = c.contains(A8)

    def without(color: Color): Castles =
      c & Bitboard.rank(color.lastRank)

    def without(color: Color, side: Side): Castles =
      c & ~color.at(side).bl

    def add(color: Color, side: Side): Castles =
      c.addSquare(color.at(side))

    def update(color: Color, kingSide: Boolean, queenSide: Boolean): Castles =
      c.without(color) | kingSide.at(color.kingSide) | queenSide.at(color.queenSide)

    def toSeq: Array[Boolean] = Array(whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide)

    inline def unary_~ : Castles                = ~c
    inline infix def &(inline o: Long): Castles = c & o
    inline infix def ^(inline o: Long): Castles = c ^ o
    inline infix def |(inline o: Long): Castles = c | o

    @targetName("andB")
    inline infix def &(o: Bitboard): Castles = c & o.value
    @targetName("xorB")
    inline infix def ^(o: Bitboard): Castles = c ^ o.value
    @targetName("orB")
    inline infix def |(o: Bitboard): Castles = c | o.value

    inline def value: Long  = c
    inline def bb: Bitboard = Bitboard(c)

    inline def contains(inline square: Square): Boolean =
      (c & (1L << square.value)) != 0L

    def addSquare(square: Square): Castles = c | square.bl

  extension (b: Boolean) inline def at(square: Square) = if b then square.bl else none

  extension (color: Color)
    inline def at(side: Side): Square =
      (color, side) match
        case (White, KingSide)  => H1
        case (White, QueenSide) => A1
        case (Black, KingSide)  => H8
        case (Black, QueenSide) => A8

    inline def kingSide: Square  = at(KingSide)
    inline def queenSide: Square = at(QueenSide)

  def apply(
      whiteKingSide: Boolean,
      whiteQueenSide: Boolean,
      blackKingSide: Boolean,
      blackQueenSide: Boolean
  ): Castles =
    whiteKingSide.at(White.kingSide) | whiteQueenSide.at(White.queenSide) |
      blackKingSide.at(Black.kingSide) | blackQueenSide.at(Black.queenSide)

  inline def apply(inline l: Long): Castles = init & l

  @targetName("applyBitboard")
  def apply(bb: Bitboard): Castles = init & bb.value

  // consider x-fen notation
  val charToSquare: (c: Char) => Option[Square] =
    case 'k' => Some(H8)
    case 'q' => Some(A8)
    case 'K' => Some(H1)
    case 'Q' => Some(A1)
    case _   => None

  val init: Castles  = 0x8100000000000081L
  val none: Castles  = 0L
  val black: Castles = 0x8100000000000000L
