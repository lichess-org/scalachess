package chess

import cats.syntax.all.*
import scala.annotation.tailrec

trait HasId[A, Id]:
  def getId(a: A): Id
  extension (a: A)
    inline def id: Id                    = getId(a)
    inline def sameId(other: A): Boolean = a.id == other.id
    inline def hasId(id: Id): Boolean    = a.id == id

  extension (xs: List[A])
    def remove(v: A): List[A] =
      xs.removeById(v.id)

    def removeById(id: Id): List[A] =
      xs.foldLeft((false, List.empty[A])) { (acc, v) =>
        if acc._1 then (acc._1, v :: acc._2)
        else if v.hasId(id) then (true, acc._2)
        else (false, v :: acc._2)
      }._2
        .reverse

trait Mergeable[A, Id] extends HasId[A, Id]:
  // laws
  // a1.sameId(a2) => Some
  // !a1.sameId(a2) => None
  // a1.merge(a2).flatMap(_.merge(a3)) == a2.merge(a3).flatMap(a1.merge(_))
  def tryMerge(a1: A, a2: A): Option[A]

  extension (a1: A) def merge(a2: A): Option[A] = tryMerge(a1, a2)

  extension (xs: List[A])

    def add(ys: List[A]): List[A] =
      ys.foldLeft(xs)(_ `add` _)

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
