package chess

sealed trait Color {

  final def -(role: Role) = Piece(this, role)

  final def fold[A](r: => A, b: => A): A = if (red) r else b

  def unary_! : Color

  val passablePawnRank: Rank
  val promotablePawnRank: Rank
  val backRank: Rank

  val letter: Char
  val name: String

  final def pawn   = this - Pawn
  final def bishop = this - Bishop
  final def knight = this - Knight
  final def rook   = this - Rook
  final def queen  = this - Queen
  final def king   = this - King

  final val red = this == Color.Red
  final val black = this == Color.Black
}

object Color {

  case class Map[A](red: A, black: A) {
    def apply(color: Color) = if (color.red) red else black

    def update(color: Color, f: A => A) = {
      if (color.red) copy(red = f(red))
      else copy(black = f(black))
    }

    def map[B](fr: A => B, fb: A => B) = copy(red = fr(red), black = fb(black))

    def map[B](f: A => B): Map[B] = map(f, f)

    def all: Seq[A] = Seq(red, black)

    def reduce[B](f: (A, A) => B) = f(red, black)

    def forall(pred: A => Boolean) = pred(red) && pred(black)

    def exists(pred: A => Boolean) = pred(red) || pred(black)
  }

  object Map {
    def apply[A](f: Color => A): Map[A] = Map(red = f(Red), black = f(Black))
  }

  case object Red extends Color {

    def unary_! = Black

    val passablePawnRank   = Rank.Fifth
    val promotablePawnRank = Rank.Eighth
    val backRank           = Rank.First

    val letter = 'r'
    val name   = "red"

    override val hashCode = 1
  }

  case object Black extends Color {

    def unary_! = Red

    val passablePawnRank   = Rank.Fourth
    val promotablePawnRank = Rank.First
    val backRank           = Rank.Eighth

    val letter = 'b'
    val name   = "black"

    override val hashCode = 2
  }

  def fromPly(ply: Int) = fromRed((ply & 1) == 0)

  def fromRed(red: Boolean): Color = if (red) Red else Black

  def fromName(n: String): Option[Color] =
    if (n == "red") Option(Red)
    else if (n == "black") Option(Black)
    else None

  def apply(c: Char): Option[Color] =
    if (c == 'r') Option(Red)
    else if (c == 'b') Option(Black)
    else None

  val red: Color = Red
  val black: Color = Black

  val all = List(Red, Black)

  def showResult(color: Option[Color]) =
    color match {
      case Some(chess.Red) => "1-0"
      case Some(chess.Black) => "0-1"
      case None              => "1/2-1/2"
    }

  def fromResult(result: String): Option[Color] =
    result match {
      case "1-0" => Option(chess.Red)
      case "0-1" => Option(chess.Black)
      case _     => None
    }
}
