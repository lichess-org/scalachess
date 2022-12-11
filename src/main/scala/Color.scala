package chess

import scala.annotation.targetName

enum Color(val name: String, val letter: Char):

  case White extends Color("white", 'w')
  case Black extends Color("black", 'b')

  lazy val white = this == Color.White
  lazy val black = this == Color.Black

  def fold[A](w: => A, b: => A): A = if white then w else b

  @targetName("negate")
  def unary_! = fold(Black, White)

  lazy val passablePawnRank: Rank   = fold(Rank.Fifth, Rank.Fourth)
  lazy val promotablePawnRank: Rank = fold(Rank.Eighth, Rank.First)
  lazy val backRank: Rank           = fold(Rank.First, Rank.Eighth)

  inline def -(inline role: Role) = Piece(this, role)

  inline def pawn   = this - Pawn
  inline def bishop = this - Bishop
  inline def knight = this - Knight
  inline def rook   = this - Rook
  inline def queen  = this - Queen
  inline def king   = this - King

  override def hashCode = fold(1, 2)

object Color:

  case class Map[A](white: A, black: A):
    def apply(color: Color) = if (color.white) white else black

    def update(color: Color, f: A => A) =
      if (color.white) copy(white = f(white))
      else copy(black = f(black))

    def map[B](fw: A => B, fb: A => B) = copy(white = fw(white), black = fb(black))

    def map[B](f: A => B): Map[B] = map(f, f)

    def all: Seq[A] = Seq(white, black)

    def reduce[B](f: (A, A) => B) = f(white, black)

    def forall(pred: A => Boolean) = pred(white) && pred(black)

    def exists(pred: A => Boolean) = pred(white) || pred(black)

  object Map:
    def apply[A](f: Color => A): Map[A] = Map(white = f(White), black = f(Black))

  inline def fromPly(inline ply: Int) = fromWhite((ply & 1) == 0)

  inline def fromWhite(inline white: Boolean): Color = if (white) White else Black

  def fromName(n: String): Option[Color] =
    if (n == "white") Option(White)
    else if (n == "black") Option(Black)
    else None

  def apply(c: Char): Option[Color] =
    if (c == 'w') Option(White)
    else if (c == 'b') Option(Black)
    else None

  val white: Color = White
  val black: Color = Black

  val all = List[Color](White, Black)
