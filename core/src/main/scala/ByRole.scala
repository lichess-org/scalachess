package chess

import cats.Functor

case class ByRole[A](pawn: A, knight: A, bishop: A, rook: A, queen: A, king: A):
  def apply(role: Role): A = role match
    case Pawn => pawn
    case Knight => knight
    case Bishop => bishop
    case Rook => rook
    case Queen => queen
    case King => king

  inline def update(role: Role, f: A => A): ByRole[A] = role match
    case Pawn => copy(pawn = f(pawn))
    case Knight => copy(knight = f(knight))
    case Bishop => copy(bishop = f(bishop))
    case Rook => copy(rook = f(rook))
    case Queen => copy(queen = f(queen))
    case King => copy(king = f(king))

  inline def find(f: A => Boolean): Option[A] =
    if f(pawn) then Some(pawn)
    else if f(knight) then Some(knight)
    else if f(bishop) then Some(bishop)
    else if f(rook) then Some(rook)
    else if f(queen) then Some(queen)
    else if f(king) then Some(king)
    else None

  inline def fold[B](z: B)(f: (B, A) => B): B =
    f(f(f(f(f(f(z, pawn), knight), bishop), rook), queen), king)

  inline def fold[B](z: B)(f: (B, Role, A) => B): B =
    f(f(f(f(f(f(z, Pawn, pawn), Knight, knight), Bishop, bishop), Rook, rook), Queen, queen), King, king)

  inline def foreach[U](f: A => U): Unit =
    f(pawn): Unit
    f(knight): Unit
    f(bishop): Unit
    f(rook): Unit
    f(queen): Unit
    f(king): Unit

  inline def foreach[U](f: (Role, A) => U): Unit =
    f(Pawn, pawn): Unit
    f(Knight, knight): Unit
    f(Bishop, bishop): Unit
    f(Rook, rook): Unit
    f(Queen, queen): Unit
    f(King, king): Unit

  inline def findRole(f: A => Boolean): Option[Role] =
    if f(pawn) then Some(Pawn)
    else if f(knight) then Some(Knight)
    else if f(bishop) then Some(Bishop)
    else if f(rook) then Some(Rook)
    else if f(queen) then Some(Queen)
    else if f(king) then Some(King)
    else None

  def values: List[A] = List(pawn, knight, bishop, rook, queen, king)

object ByRole:

  def fill[A](a: A): ByRole[A] = ByRole(a, a, a, a, a, a)

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
