package chess

import cats.Eq
import cats.derived.*

import scala.annotation.targetName

enum Color(val name: String, val letter: Char) derives Eq:

  case White extends Color("white", 'w')
  case Black extends Color("black", 'b')

  lazy val white = this == Color.White
  lazy val black = this == Color.Black

  inline def fold[A](inline w: A, inline b: A): A = if white then w else b

  @targetName("negate")
  def unary_! = fold(Black, White)

  lazy val backRank: Rank           = fold(Rank.First, Rank.Eighth)
  lazy val thirdRank: Rank          = fold(Rank.Third, Rank.Sixth)
  lazy val fourthRank: Rank         = fold(Rank.Fourth, Rank.Fifth)
  lazy val fifthRank: Rank          = fold(Rank.Fifth, Rank.Fourth)
  lazy val sixthRank: Rank          = fold(Rank.Sixth, Rank.Third)
  lazy val seventhRank: Rank        = fold(Rank.Seventh, Rank.Second)
  lazy val lastRank: Rank           = fold(Rank.Eighth, Rank.First)
  lazy val passablePawnRank: Rank   = fifthRank
  lazy val promotablePawnRank: Rank = lastRank

  inline def -(inline role: Role) = Piece(this, role)

  inline def pawn   = this - Pawn
  inline def bishop = this - Bishop
  inline def knight = this - Knight
  inline def rook   = this - Rook
  inline def queen  = this - Queen
  inline def king   = this - King

  override def hashCode = fold(1, 2)

object Color:

  def fromName(n: String): Option[Color] =
    if n == "white" then Option(White)
    else if n == "black" then Option(Black)
    else None

  def apply(c: Char): Option[Color] =
    if c == 'w' then Option(White)
    else if c == 'b' then Option(Black)
    else None

  val white: Color = White
  val black: Color = Black

  val all = List(White, Black)

  inline def fromWhite(inline white: Boolean): Color = if white then White else Black
