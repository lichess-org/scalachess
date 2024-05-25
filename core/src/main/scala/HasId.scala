package chess

import cats.syntax.all.*

import scala.annotation.tailrec

trait HasId[A, Id]:
  extension (a: A)
    def id: Id
    inline def sameId(other: A): Boolean = a.id == other.id
    inline def hasId(id: Id): Boolean    = a.id == id

  extension (xs: List[A])
    def remove(v: A): List[A] =
      xs.removeById(v.id)

    def removeById(id: Id): List[A] =
      xs match
        case (v :: vs) if v.hasId(id) => vs
        case (v :: vs)                => v :: vs.removeById(id)
        case Nil                      => Nil

trait Mergeable[A]:

  extension (a: A)

    // laws
    // a1.sameId(a2) => Some
    // !a1.sameId(a2) => None
    // a1.merge(a2).flatMap(_.merge(a3)) == a2.merge(a3).flatMap(a1.merge(_))
    def merge(other: A): Option[A]

    // laws
    // canMerge == merge.isDefined
    def canMerge[Id](other: A): HasId[A, Id] ?=> Boolean = a.sameId(other)

  extension (xs: List[A])

    def add(ys: List[A]): List[A] =
      ys.foldLeft(xs)(_.add(_))

    def add(v: A): List[A] =
      @tailrec
      def loop(acc: List[A], rest: List[A]): List[A] =
        rest match
          case Nil => acc :+ v
          case y :: ys =>
            y.merge(v) match
              case Some(m) => acc ++ (m +: ys)
              case _       => loop(acc :+ y, ys)

      loop(Nil, xs)

    // merge all elements that can be merged together
    def merge: List[A] =
      Nil.add(xs)
