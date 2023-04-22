package chess
package format
package pgn

import cats.*
import cats.derived.*
import cats.syntax.all.*

import monocle.{ Optional, Traversal }

/**
 * Node is a tree structure specialized for chess games.
 * It's a tree with a main line and variations.
 * Node is a [Functor](https://typelevel.org/cats/typeclasses/functor.html)
 * Node is a [Traverse](https://typelevel.org/cats/typeclasses/traverse.html)
 * It is also provided some Monocle optics see Node.filterOptional and Node.filterTraversal
  */
case class Node[A](
    value: A,
    child: Option[Node[A]], // main line next move
    // alternate moves in linked list form, e.g. Node("e4", None, Some("d4", None, Some("Nf3")))
    variation: Option[Node[A]]
) derives Functor,
      Traverse:

  def mainLine: List[A]                    = value :: child.fold(Nil)(_.mainLine)
  def variations: List[Node[A]]            = variation.fold(Nil)(v => v :: v.variations)
  def totalNodes: Int                      = this.foldLeft(0)((b, _) => b + 1)
  def mainLineAndVariations: List[Node[A]] = child.map(_ :: variations).getOrElse(variations)
  def children: List[Node[A]]              = child.fold(Nil)(c => c :: c.variations)

  // // find the first Node that statisfies the predicate
  // def findNode(predicate: A => Boolean): Option[Node[A]] =
  //   if predicate(value) then this.some
  //   else mainLineAndVariations.foldLeft(none[Node[A]])((b, n) => b.orElse(n.findNode(predicate)))

  // def replaceNode(predicate: A => Boolean)(node: Node[A]): Option[Node[A]] =
  //   modifyNode(predicate)(_ => node)

  // // modify the first node that satisfies the predicate (dfs with main line and then variations)
  // def modifyNode(predicate: A => Boolean)(f: Node[A] => Node[A]): Option[Node[A]] =
  //   if predicate(value) then f(this).some
  //   else
  //     child.flatMap(_.modifyNode(predicate)(f)) match
  //       case Some(n) => copy(child = Some(n)).some
  //       case None =>
  //         variations.foldLeft((false, List.empty[Node[A]])) {
  //           case ((true, acc), n) => (true, acc :+ n)
  //           case ((false, acc), n) =>
  //             n.modifyNode(predicate)(f) match
  //               case Some(nn) => (true, acc :+ nn)
  //               case None     => (false, acc :+ n)
  //         } match
  //           case (true, ns) => copy(variations = ns).some
  //           case (false, _) => none

  // // delete the first node that satisfies the predicate (both child and variations)
  // // except the root
  // def deleteSubNode(predicate: A => Boolean): Option[Node[A]] =
  //   child.flatMap { n =>
  //     if predicate(n.value) then copy(child = None).some
  //     else n.deleteSubNode(predicate).map(nn => this.copy(child = Some(nn)))
  //   } match
  //     case Some(n) => n.some
  //     case None =>
  //       variations.foldLeft((false, List.empty[Node[A]])) {
  //         case ((true, acc), n) => (true, acc :+ n)
  //         case ((false, acc), n) =>
  //           if predicate(n.value) then (true, acc)
  //           else
  //             n.deleteSubNode(predicate) match
  //               case Some(nn) => (true, acc :+ nn)
  //               case None     => (false, acc :+ n)
  //       } match
  //         case (true, ns) => copy(variations = ns).some
  //         case (false, _) => none

object Node:

  def filterTraversal[A](predicate: A => Boolean): Traversal[Node[A], A] = new:
    def modifyA[F[_]: Applicative](f: A => F[A])(s: Node[A]): F[Node[A]] =
      s.map(a => if predicate(a) then f(a) else a.pure[F]).sequence

  // def filterOptional[A](predicate: A => Boolean): Optional[Node[A], Node[A]] =
  //   Optional[Node[A], Node[A]](x => x.findNode(predicate))(x => n => n.replaceNode(predicate)(x).getOrElse(x))
