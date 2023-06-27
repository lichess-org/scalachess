package chess

import cats.Functor
import cats.syntax.all.*
import chess.bitboard.Bitboard

sealed trait Role:
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val pgn: Char          = forsythUpper
  lazy val name               = toString.toLowerCase

sealed trait PromotableRole extends Role

/** Promotable in antichess. */
case object King extends PromotableRole:
  val forsyth = 'k'

case object Queen extends PromotableRole:
  val forsyth = 'q'

case object Rook extends PromotableRole:
  val forsyth = 'r'

case object Bishop extends PromotableRole:
  val forsyth = 'b'

case object Knight extends PromotableRole:
  val forsyth = 'n'

case object Pawn extends Role:
  val forsyth = 'p'

object Role:

  val all: List[Role]                                   = List(King, Queen, Rook, Bishop, Knight, Pawn)
  val allPromotable: List[PromotableRole]               = List(Queen, Rook, Bishop, Knight, King)
  val allByForsyth: Map[Char, Role]                     = all.mapBy(_.forsyth)
  val allByPgn: Map[Char, Role]                         = all.mapBy(_.pgn)
  val allByName: Map[String, Role]                      = all.mapBy(_.name)
  val allPromotableByName: Map[String, PromotableRole]  = allPromotable.mapBy(_.toString)
  val allPromotableByForsyth: Map[Char, PromotableRole] = allPromotable.mapBy(_.forsyth)
  val allPromotableByPgn: Map[Char, PromotableRole]     = allPromotable.mapBy(_.pgn)

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable

  def valueOf(r: Role): Option[Int] =
    r match
      case Pawn   => Option(1)
      case Knight => Option(3)
      case Bishop => Option(3)
      case Rook   => Option(5)
      case Queen  => Option(9)
      case King   => None

case class ByRole[A](pawn: A, knight: A, bishop: A, rook: A, queen: A, king: A):
  def apply(role: Role): A = role match
    case Pawn   => pawn
    case Knight => knight
    case Bishop => bishop
    case Rook   => rook
    case Queen  => queen
    case King   => king

  def update(role: Role, f: A => A): ByRole[A] = role match
    case Pawn   => copy(pawn = f(pawn))
    case Knight => copy(knight = f(knight))
    case Bishop => copy(bishop = f(bishop))
    case Rook   => copy(rook = f(rook))
    case Queen  => copy(queen = f(queen))
    case King   => copy(king = f(king))

  def find(f: A => Boolean): Option[A] =
    if f(pawn) then Some(pawn)
    else if f(knight) then Some(knight)
    else if f(bishop) then Some(bishop)
    else if f(rook) then Some(rook)
    else if f(queen) then Some(queen)
    else if f(king) then Some(king)
    else None

  def fold[B](z: B)(f: (B, A) => B): B =
    f(f(f(f(f(f(z, pawn), knight), bishop), rook), queen), king)

  def fold[B](z: B)(f: (B, Role, A) => B): B =
    f(f(f(f(f(f(z, Pawn, pawn), Knight, knight), Bishop, bishop), Rook, rook), Queen, queen), King, king)

  def findRole(f: A => Boolean): Option[Role] =
    if f(pawn) then Some(Pawn)
    else if f(knight) then Some(Knight)
    else if f(bishop) then Some(Bishop)
    else if f(rook) then Some(Rook)
    else if f(queen) then Some(Queen)
    else if f(king) then Some(King)
    else None

  def values: List[A] = List(pawn, knight, bishop, rook, queen, king)

object ByRole:

  def apply[A](a: A): ByRole[A] = ByRole(a, a, a, a, a, a)

  given Functor[ByRole] with
    def map[A, B](byRole: ByRole[A])(f: A => B): ByRole[B] =
      ByRole(
        f(byRole.pawn),
        f(byRole.knight),
        f(byRole.bishop),
        f(byRole.rook),
        f(byRole.queen),
        f(byRole.king)
      )

  extension (byRole: ByRole[Bitboard])
    def discard(mask: Bitboard): ByRole[Bitboard] =
      val notMask = ~mask
      byRole.map(_ & notMask)
