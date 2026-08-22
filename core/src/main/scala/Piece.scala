package chess

import cats.Eq

case class Piece private (color: Color, role: Role):

  def is(c: Color): Boolean = c == color
  def is(r: Role): Boolean = r == role
  def isNot(r: Role): Boolean = r != role
  def unary_! : Piece = Piece(!color, role)

  def forsyth: Char = if color.white then role.forsythUpper else role.forsyth

  // the piece at from can attack the target to when mask are all the occupied squares
  def eyes(from: Square, to: Square, mask: Bitboard): Boolean =
    role match
      case King => from.kingAttacks.contains(to)
      case Queen => from.queenAttacks(mask).contains(to)
      case Rook => from.rookAttacks(mask).contains(to)
      case Bishop => from.bishopAttacks(mask).contains(to)
      case Knight => from.knightAttacks.contains(to)
      case Pawn => from.pawnAttacks(color).contains(to)

  override def toString = s"$color-$role".toLowerCase

object Piece:

  given Eq[Piece] = Eq.fromUniversalEquals[Piece]

  val WhitePawn: Piece = new Piece(White, Pawn)
  val WhiteKnight: Piece = new Piece(White, Knight)
  val WhiteBishop: Piece = new Piece(White, Bishop)
  val WhiteRook: Piece = new Piece(White, Rook)
  val WhiteQueen: Piece = new Piece(White, Queen)
  val WhiteKing: Piece = new Piece(White, King)
  val BlackPawn: Piece = new Piece(Black, Pawn)
  val BlackKnight: Piece = new Piece(Black, Knight)
  val BlackBishop: Piece = new Piece(Black, Bishop)
  val BlackRook: Piece = new Piece(Black, Rook)
  val BlackQueen: Piece = new Piece(Black, Queen)
  val BlackKing: Piece = new Piece(Black, King)

  def apply(color: Color, role: Role): Piece =
    if color.white then
      role match
        case Pawn => WhitePawn
        case Knight => WhiteKnight
        case Bishop => WhiteBishop
        case Rook => WhiteRook
        case Queen => WhiteQueen
        case King => WhiteKing
    else
      role match
        case Pawn => BlackPawn
        case Knight => BlackKnight
        case Bishop => BlackBishop
        case Rook => BlackRook
        case Queen => BlackQueen
        case King => BlackKing

  private val allByFen: Map[Char, Piece] =
    Map(
      'P' -> WhitePawn,
      'N' -> WhiteKnight,
      'B' -> WhiteBishop,
      'R' -> WhiteRook,
      'Q' -> WhiteQueen,
      'K' -> WhiteKing,
      'p' -> BlackPawn,
      'n' -> BlackKnight,
      'b' -> BlackBishop,
      'r' -> BlackRook,
      'q' -> BlackQueen,
      'k' -> BlackKing
    )

  def fromChar(c: Char): Option[Piece] =
    allByFen.get(c)
