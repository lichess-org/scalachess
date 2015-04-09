package chess

sealed trait Color {

  def -(role: Role) = Piece(this, role)

  def fold[A](w: => A, b: => A): A = if (white) w else b

  val unary_! : Color

  val passablePawnY: Int
  val promotablePawnY: Int

  val letter: Char
  val name: String

  def pawn = this - Pawn
  def bishop = this - Bishop
  def knight = this - Knight
  def rook = this - Rook
  def queen = this - Queen
  def king = this - King

  val white = this == Color.White
  val black = this == Color.Black

  override def toString = name
}

object Color {

  case class Map[A](white: A, black: A) {
    def apply(color: Color) = if (color.white) white else black
  }

  object Map {
    def apply[A](f: Color => A): Map[A] = Map(white = f(White), black = f(Black))
  }

  case object White extends Color {

    lazy val unary_! = Black

    val passablePawnY = 5
    val promotablePawnY = 8

    val letter = 'w'
    val name = "white"
  }

  case object Black extends Color {

    lazy val unary_! = White

    val passablePawnY = 4
    val promotablePawnY = 1

    val letter = 'b'
    val name = "black"
  }

  def apply(b: Boolean): Color = if (b) White else Black

  def apply(n: String): Option[Color] =
    if (n == "white") Some(White)
    else if (n == "black") Some(Black)
    else None

  def apply(c: Char): Option[Color] =
    if (c == 'w') Some(White)
    else if (c == 'b') Some(Black)
    else None

  val white: Color = White
  val black: Color = Black

  val all = List(White, Black)

  val names = all map (_.name)

  def exists(name: String) = all exists (_.name == name)
}
