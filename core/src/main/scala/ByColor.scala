package chess

import alleycats.Zero
import cats.syntax.all.*
import cats.{ Applicative, Eq, Eval, FlatMap, Functor, Monoid, Semigroupal, Traverse }

import scala.annotation.targetName
import scala.util.NotGiven

case class ByColor[A](white: A, black: A):

  inline def apply(inline color: Color): A = if color.white then white else black

  inline def apply[B](inline color: Color)(f: A => B): B = if color.white then f(white) else f(black)

  inline def update(inline color: Color, f: A => A): ByColor[A] =
    if color.white then copy(white = f(white))
    else copy(black = f(black))

  inline def update(inline color: Color, f: A => Option[A]): Option[ByColor[A]] =
    if color.white then f(white).map(w => copy(white = w))
    else f(black).map(b => copy(black = b))

  def map[B](fw: A => B, fb: A => B): ByColor[B] = ByColor(fw(white), fb(black))

  def map[B](f: A => B): ByColor[B]                 = map(f, f)
  def mapList[B](f: A => B): List[B]                = List(f(white), f(black))
  def mapReduce[B, C](f: A => B)(r: (B, B) => C): C = r(f(white), f(black))

  def mapWithColor[B](f: (Color, A) => B): ByColor[B] = ByColor(f(White, white), f(Black, black))

  def zip[B](other: ByColor[B]): ByColor[(A, B)] = ByColor((white, other.white), (black, other.black))
  def zip[B, C](other: ByColor[B], f: (A, B) => C): ByColor[C] =
    ByColor(f(white, other.white), f(black, other.black))
  def zipColor: ByColor[(Color, A)] = ByColor((White, white), (Black, black))
  def toPair: (A, A)                = (white, black)

  lazy val all: List[A] = List(white, black)

  def reduce[B](f: (A, A) => B): B = f(white, black)

  def fold[B](init: B)(f: (B, A) => B): B        = f(f(init, white), black)
  def fold[B](init: B)(f: (B, Color, A) => B): B = f(f(init, White, white), Black, black)

  def foreach[U](f: A => U): Unit =
    f(white): Unit
    f(black): Unit

  def foreach[U](f: (Color, A) => U): Unit =
    f(White, white): Unit
    f(Black, black): Unit

  def forall(pred: A => Boolean): Boolean = pred(white) && pred(black)

  def exists(pred: A => Boolean): Boolean = pred(white) || pred(black)

  def swap: ByColor[A] = copy(white = black, black = white)

  inline def findColor(pred: A => Boolean): Option[Color] =
    if pred(white) then White.some
    else if pred(black) then Black.some
    else None

  inline def find(pred: A => Boolean): Option[A] =
    if pred(white) then white.some
    else if pred(black) then black.some
    else None

  @targetName("findOption")
  def find[B](f: A => Option[B]): Option[B] =
    f(white).orElse(f(black))

  def collect[B](f: PartialFunction[A, B]): Option[B] =
    f.lift(white).orElse(f.lift(black))

  def contains(a: A): Eq[A] ?=> Boolean =
    exists(_ === a)

  def flatMap[B](f: A => IterableOnce[B]): List[B] =
    val b = List.newBuilder[B]
    b ++= f(white)
    b ++= f(black)
    b.result()

  def flatten[B](using toIterableOnce: A => IterableOnce[B]): List[B] =
    val b = List.newBuilder[B]
    b ++= toIterableOnce(white)
    b ++= toIterableOnce(black)
    b.result()

  def traverse[F[_], B](f: A => F[B]): Applicative[F] ?=> F[ByColor[B]] =
    (f(white), f(black)).mapN(ByColor(_, _))

  def traverseReduce[F[_], B, C](f: A => F[B])(r: (B, B) => C): Applicative[F] ?=> F[C] =
    (f(white), f(black)).mapN(r)

object ByColor:
  inline def fill[A](a: A): ByColor[A]          = ByColor(a, a)
  inline def fromPair[A](p: (A, A)): ByColor[A] = ByColor(p._1, p._2)

  @annotation.nowarn("msg=unused implicit parameter")
  def apply[A](f: Color => A)(using NotGiven[f.type <:< PartialFunction[Color, A]]): ByColor[A] =
    ByColor(white = f(White), black = f(Black))

  def apply[F[_], A](f: Color => F[A]): Applicative[F] ?=> F[ByColor[A]] =
    (f(White), f(Black)).mapN(ByColor(_, _))

  given [A: Eq]: Eq[ByColor[A]] with
    def eqv(x: ByColor[A], y: ByColor[A]) =
      x.white === y.white && x.black === y.black

  given [A: Zero]: Zero[ByColor[A]] with
    def zero = ByColor.fill(Zero[A].zero)

  given [A: Monoid]: Monoid[ByColor[A]] with
    def empty = ByColor.fill(Monoid[A].empty)
    def combine(x: ByColor[A], y: ByColor[A]) =
      ByColor(Monoid[A].combine(x.white, y.white), Monoid[A].combine(x.black, y.black))

  given Functor[ByColor] with Applicative[ByColor] with Traverse[ByColor] with

    override def map[A, B](fa: ByColor[A])(f: A => B): ByColor[B] = fa.map(f)

    override def foldLeft[A, B](fa: ByColor[A], b: B)(f: (B, A) => B): B =
      fa.fold(b)(f)

    override def foldRight[A, B](fa: ByColor[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Eval.defer(f(fa.white, Eval.defer(f(fa.black, lb))))

    def traverse[G[_]: Applicative, A, B](fa: ByColor[A])(f: A => G[B]): G[ByColor[B]] =
      fa.traverse(f)

    def pure[A](a: A): ByColor[A] = ByColor.fill(a)
    def ap[A, B](ff: ByColor[A => B])(fa: ByColor[A]): ByColor[B] =
      ByColor(ff.white(fa.white), ff.black(fa.black))

  extension [F[_], A](bc: ByColor[F[A]])

    def mapN[Z](f: (A, A) => Z)(using Functor[F], Semigroupal[F]): F[Z] =
      Semigroupal.map2(bc.white, bc.black)(f)

    def flatMapN[Z](f: (A, A) => F[Z])(using flatMap: FlatMap[F]): F[Z] =
      flatMap.flatMap2(bc.white, bc.black)(f)

    def tupled(using Applicative[F]): F[(A, A)] =
      (bc.white, bc.black).tupled
