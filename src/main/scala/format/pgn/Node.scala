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

  lazy val mainline: List[A]                 = value :: child.fold(Nil)(_.mainline)
  lazy val variations: List[Node[A]]         = variation.fold(Nil)(v => v :: v.variations)
  lazy val childAndVariations: List[Node[A]] = child.map(_ :: variations).getOrElse(variations)

  // Akin to map, but allows to keep track of a state value when calling the function.
  def mapAccuml[S, B](init: S)(f: (S, A) => (S, B)): (S, Node[B]) =
    val (s1, b) = f(init, value)
    val v       = variation.map(_.mapAccuml(init)(f)._2)
    child.map(_.mapAccuml(s1)(f)) match
      case None    => (s1, Node(b, None, v))
      case Some(s) => (s._1, Node(b, s._2.some, v))

  def mapAccuml_[S, B](init: S)(f: (S, A) => (S, B)): Node[B] =
    mapAccuml(init)(f)._2

  // Akin to mapAccuml, return an Option[Node[B]]
  // when a node from mainline returns None, we stop traverse down that line
  // when a variatioon node returns None, we just ignore it
  // TODO: now if the f(value) is None, the whole tree is None
  // should we promote a variation to mainline if the f(value) is None?
  def _mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): (S, Option[Node[B]]) =
    f(init, value) match
      case (s1, None) => (s1, None)
      case (s1, Some(b)) =>
        val vs = variation.map(_._mapAccumlOption(init)(f)._2).flatten
        child.map(_._mapAccumlOption(s1)(f)) match
          case None    => (s1, Node(b, None, vs).some)
          case Some(s) => (s._1, Node(b, s._2, vs).some)

  def mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): Option[Node[B]] =
    _mapAccumlOption(init)(f)._2

  // find the first Node that statisfies the predicate
  def findNode(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else childAndVariations.foldLeft(none[Node[A]])((b, n) => b.orElse(n.findNode(predicate)))

  // find the first variation that statisfies the predicate
  def findVariation(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else
      variation.fold(none[Node[A]]): v =>
        if predicate(v.value) then v.some
        else v.findVariation(predicate)

  // find node in the mainline
  def findMainlineNode(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else
      child.fold(none[Node[A]]): c =>
        if predicate(c.value) then c.some
        else c.findMainlineNode(predicate)

  def lastMainlineNode: Node[A] =
    child.fold(this)(_.lastMainlineNode)

  def modifyLastMainlineNode(f: Node[A] => Node[A]): Node[A] =
    child.fold(this)(c => copy(child = Some(c.modifyLastMainlineNode(f))))

  def findChildOrVariation(predicate: A => Boolean): Option[Node[A]] =
    childAndVariations.foldLeft(none[Node[A]]): (acc, v) =>
      if acc.isDefined then acc
      else if predicate(v.value) then v.some
      else None

  def replaceNode(predicate: A => Boolean)(node: Node[A]): Option[Node[A]] =
    modifyNode(predicate)(_ => node)

  // modify the first node that satisfies the predicate (dfs with main line and then variations)
  def modifyNode(predicate: A => Boolean)(f: Node[A] => Node[A]): Option[Node[A]] =
    if predicate(value) then f(this).some
    else
      child.flatMap(_.modifyNode(predicate)(f)) match
        case Some(n) => copy(child = Some(n)).some
        case None =>
          variation.flatMap(_.modifyNode(predicate)(f)) match
            case Some(n) => copy(variation = Some(n)).some
            case None    => None

  // // delete the first node that satisfies the predicate (both child and variations)
  // // except the root
  def deleteSubNode(predicate: A => Boolean): Option[Node[A]] =
    child.flatMap { n =>
      if predicate(n.value) then copy(child = None).some
      else n.deleteSubNode(predicate).map(nn => this.copy(child = Some(nn)))
    } match
      case Some(n) => n.some
      case None =>
        variation.flatMap { n =>
          if predicate(n.value) then copy(variation = None).some
          else n.deleteSubNode(predicate).map(nn => this.copy(variation = Some(nn)))
        } match
          case Some(n) => n.some
          case None    => None

object Node:

  def filterTraversal[A](predicate: A => Boolean): Traversal[Node[A], A] = new:
    def modifyA[F[_]: Applicative](f: A => F[A])(s: Node[A]): F[Node[A]] =
      s.map(a => if predicate(a) then f(a) else a.pure[F]).sequence

  def filterOptional[A](predicate: A => Boolean): Optional[Node[A], Node[A]] =
    Optional[Node[A], Node[A]](x => x.findNode(predicate))(x => n => n.replaceNode(predicate)(x).getOrElse(x))

  // def filterVariation[A](predicate: A => Boolean): Optional[Node[A], Node[A]] =
  //   Optional[Node[A], Node[A]](x => x.findNode(predicate))(x => n => n.replaceNode(predicate)(x).getOrElse(x))

  extension [A](xs: List[Node[A]])
    def toVariations: Option[Node[A]] =
      xs.reverse.foldLeft(none[Node[A]])((acc, x) => x.copy(variation = acc).some)

    def toChild: Option[Node[A]] =
      xs.reverse.foldLeft(none[Node[A]])((acc, x) => x.copy(child = acc).some)

  extension [A](xs: List[A])
    def toVariations[B](f: A => Node[B]) =
      xs.reverse.foldLeft(none[Node[B]])((acc, x) => f(x).copy(variation = acc).some)

    def toChild[B](f: A => Node[B]) =
      xs.reverse.foldLeft(none[Node[B]])((acc, x) => f(x).copy(child = acc).some)

  extension [A](o: Option[Node[A]])
    def mergeVariations(other: Option[Node[A]]): Option[Node[A]] =
      o.fold(other)(n => n.copy(variation = n.variation.mergeVariations(other)).some)
