package chess

sealed trait Color {

  def -(role: Role) = Piece(this, role)

  def fold[A](w: => A, b: => A): A = if (white) w else b

  val unary_! : Color

  val passablePawnY: Int
  val promotablePawnY: Int
  val backrankY: Int

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
}

object Color {

  case class Map[A](white: A, black: A) {
    def apply(color: Color) = if (color.white) white else black

    def update(color: Color, f: A => A) = {
      if (color.white) copy(white = f(white))
      else copy(black = f(black))
    }

    def map[B](fw: A => B, fb: A => B) = copy(white = fw(white), black = fb(black))

    def map[B](f: A => B): Map[B] = map(f, f)

    def forall(pred: A => Boolean) = pred(white) && pred(black)

    def exists(pred: A => Boolean) = pred(white) || pred(black)
  }

  object Map {
    def apply[A](f: Color => A): Map[A] = Map(white = f(White), black = f(Black))
  }

  case object White extends Color {

    lazy val unary_! = Black

    val passablePawnY = 5
    val promotablePawnY = 8
    val backrankY = 1

    val letter = 'w'
    val name = "white"

    override val hashCode = 1
  }

  case object Black extends Color {

    val unary_! = White

    val passablePawnY = 4
    val promotablePawnY = 1
    val backrankY = 8

    val letter = 'b'
    val name = "black"

    override val hashCode = 2
  }

  def fromPly(ply: Int) = apply((ply & 1) == 0)

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

  def showResult(color: Option[Color]) = color match {
    case Some(chess.White) => "1-0"
    case Some(chess.Black) => "0-1"
    case None => "½-½"
  }
}
