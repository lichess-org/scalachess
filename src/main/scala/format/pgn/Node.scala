package chess
package format
package pgn

import cats.syntax.all.*
import cats.*

import monocle.{ Optional, Traversal }

// question: what if a variation without moves?
// answer: it's possible but we need to support null move first
case class Node[A](
    value: A,
    child: Option[Node[A]],   // main line next move
    variations: List[Node[A]] // alternate moves, e.g. Node("e4", None, List("d4", "Nf3"))
):
  def mainLine: List[A] = value :: child.fold(Nil)(_.mainLine)
  def totalNodes: Int   = this.foldLeft(0)((b, _) => b + 1)

  // find the first Node that statisfies the predicate
  def findNode(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else
      child
        .flatMap(_.findNode(predicate))
        .orElse(variations.foldLeft(none[Node[A]])((b, n) => b.orElse(n.findNode(predicate))))

  def replaceNode(predicate: A => Boolean)(node: Node[A]): Option[Node[A]] =
    modifyNode(predicate)(_ => node)

  // modify the first node that satisfies the predicate (dfs with main line and then variations)
  def modifyNode(predicate: A => Boolean)(f: Node[A] => Node[A]): Option[Node[A]] =
    if predicate(value) then f(this).some
    else
      child.flatMap(_.modifyNode(predicate)(f)) match
        case Some(n) => copy(child = Some(n)).some
        case None =>
          variations.foldLeft((false, List.empty[Node[A]])) {
            case ((true, acc), n) => (true, acc :+ n)
            case ((false, acc), n) =>
              n.modifyNode(predicate)(f) match
                case Some(nn) => (true, acc :+ nn)
                case None     => (false, acc :+ n)
          } match
            case (true, ns) => copy(variations = ns).some
            case (false, _) => none

  // delete the first node that satisfies the predicate (both child and variations)
  // except the root
  def deleteSubNode(predicate: A => Boolean): Option[Node[A]] =
    child.flatMap { n =>
      if predicate(n.value) then copy(child = None).some
      else n.deleteSubNode(predicate).map(nn => this.copy(child = Some(nn)))
    } match
      case Some(n) => n.some
      case None =>
        variations.foldLeft((false, List.empty[Node[A]])) {
          case ((true, acc), n) => (true, acc :+ n)
          case ((false, acc), n) =>
            if predicate(n.value) then (true, acc)
            else
              n.deleteSubNode(predicate) match
                case Some(nn) => (true, acc :+ nn)
                case None     => (false, acc :+ n)
        } match
          case (true, ns) => copy(variations = ns).some
          case (false, _) => none

  // search and replace a node that satisfies the predicate (both child and variations)
  def setChild(predicate: A => Boolean)(node: Node[A]): Node[A] =
    if predicate(value) then copy(child = Some(node))
    else copy(child = child.map(_.setChild(predicate)(node)))

  def removeChild(predicate: A => Boolean): Node[A] =
    if predicate(value) then copy(child = None)
    else copy(child = child.map(_.removeChild(predicate)))

  def modifyChild(predicate: A => Boolean)(f: A => A): Node[A] =
    if predicate(value) then copy(value = f(value))
    else copy(child = child.map(_.modifyChild(predicate)(f)))

  def setVariations(predicate: A => Boolean)(variations: List[Node[A]]): Node[A] =
    if predicate(value) then copy(variations = variations)
    else copy(variations = variations.map(_.setVariations(predicate)(variations)))

  def modifyVariations(predicate: A => Boolean)(f: List[Node[A]] => List[Node[A]]): Node[A] =
    if predicate(value) then copy(variations = f(variations))
    else copy(variations = variations.map(_.modifyVariations(predicate)(f)))

  def removeVariations(predicate: A => Boolean): Node[A] =
    if predicate(value) then copy(variations = variations.map(_.removeVariations(predicate)))
    else copy(variations = variations.map(_.removeVariations(predicate)))

object Node:
  given Functor[Node] with
    def map[A, B](fa: Node[A])(f: A => B): Node[B] =
      fa.copy(value = f(fa.value), child = fa.child.map(_.map(f)), variations = fa.variations.map(_.map(f)))

  given Traverse[Node] with
    def traverse[G[_]: Applicative, A, B](fa: Node[A])(f: A => G[B]): G[Node[B]] =
      val g = f(fa.value)
      val h = fa.child.traverse(_.traverse(f))
      val i = fa.variations.traverse(_.traverse(f))
      (g, h, i).mapN(Node(_, _, _))

    def foldLeft[A, B](fa: Node[A], b: B)(f: (B, A) => B): B =
      val b1 = f(b, fa.value)
      val b2 = fa.child.foldLeft(b1)((b, a) => foldLeft(a, b)(f))
      fa.variations.foldLeft(b2)((b, a) => a.foldLeft(b)(f))

    def foldRight[A, B](fa: Node[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      val b1: Eval[B] = fa.variations.foldr(lb)((a, b) => a.foldRight(b)(f))
      val b2: Eval[B] = fa.child.foldr(b1)((a, b) => a.foldRight(b)(f))
      Eval.defer(f(fa.value, b2))

  def filterTraversal[A](predicate: A => Boolean): Traversal[Node[A], A] = new:
    def modifyA[F[_]: Applicative](f: A => F[A])(s: Node[A]): F[Node[A]] =
      s.map(a => if predicate(a) then f(a) else a.pure[F]).sequence

  def filterOptional[A](predicate: A => Boolean): Optional[Node[A], Node[A]] =
    Optional[Node[A], Node[A]](x => x.findNode(predicate))(x => n => n.replaceNode(predicate)(x).getOrElse(x))