package chess
package format
package pgn

import cats.*
import cats.derived.*
import cats.syntax.all.*

import monocle.{ Optional, Traversal }
import scala.annotation.tailrec

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

  lazy val mainline: List[Node[A]]           = withoutChild :: child.fold(Nil)(_.mainline)
  lazy val variations: List[Node[A]]         = variation.fold(Nil)(v => v :: v.variations)
  lazy val childAndVariations: List[Node[A]] = child.map(_ :: variations).getOrElse(variations)

  final def withValue(v: A) = copy(value = v)

  final def findPath[Id](path: List[Id])(using h: HasId[A, Id]): List[Node[A]] =
    @tailrec
    def loop(node: Node[A], path: List[Id], acc: List[Node[A]]): List[Node[A]] =
      path match
        case Nil => acc
        case head :: rest if h.getId(node.value) == head =>
          rest match
            case Nil => node :: acc
            case _ =>
              node.child match
                case Some(child) => loop(child, rest, node :: acc)
                case None        => acc
        case _ =>
          node.variation match
            case Some(variation) => loop(variation, path, acc)
            case None            => acc

    loop(this, path, Nil).reverse

  final def find[Id](path: List[Id])(using h: HasId[A, Id]): Option[Node[A]] =
    findPath(path).lastOption

  final def find[Id](id: Id)(using h: HasId[A, Id]): Option[Node[A]] =
    findNode(h.getId(_) == id)

  // modify the node with path
  // path.isEmpty returns None
  // if path is not found, return None
  final def modifyAt[Id](path: List[Id], f: Node[A] => Node[A])(using h: HasId[A, Id]): Option[Node[A]] =
    path match
      case Nil                                   => None
      case head :: Nil if h.getId(value) == head => f(this).some
      case head :: rest if h.getId(value) == head =>
        child.flatMap(_.modifyAt(rest, f)) match
          case None    => None
          case Some(c) => withChild(c).some
      case _ =>
        variation.flatMap(_.modifyAt(path, f)) match
          case None    => None
          case Some(v) => withVariations(v).some

  // delete the node at the end of the path
  // return None if path is not found
  // return Some(None) if the node is deleted
  final def deleteAt[Id](path: List[Id])(using h: HasId[A, Id]): Option[Option[Node[A]]] =
    path match
      case Nil                                   => None
      case head :: Nil if h.getId(value) == head => variation.some
      case head :: rest if h.getId(value) == head =>
        child.flatMap(_.deleteAt(rest)) match
          case None    => None
          case Some(c) => copy(child = c).some.some
      case _ =>
        variation.flatMap(_.deleteAt(path)) match
          case None    => None
          case Some(v) => copy(variation = v).some.some

  final def findChild[Id](path: List[Id])(using h: HasId[A, Id]): Option[Node[A]] =
    path match
      case Nil => None
      case head :: rest if h.getId(value) == head =>
        rest match
          case Nil => child
          case _   => child.flatMap(_.findChild(rest))
      case _ =>
        variation.flatMap(_.findChild(path))

  final def modifyChild[Id](path: List[Id], f: Node[A] => Node[A])(using h: HasId[A, Id]): Option[Node[A]] =
    path match
      case Nil => None
      case head :: rest if h.getId(value) == head =>
        rest match
          case Nil => child.map(f).map(withChild)
          case _   => child.flatMap(_.modifyChild(rest, f)).map(withChild)
      case _ =>
        variation.flatMap(_.modifyChild(path, f))

  // replace child for the node at the end of the path
  // if path.isEmpty means
  final def replaceChildAt[Id](node: Node[A], path: List[Id])(using h: HasId[A, Id]): Option[Node[A]] = ???

  // Akin to map, but allows to keep track of a state value when calling the function.
  final def mapAccuml[S, B](init: S)(f: (S, A) => (S, B)): (S, Node[B]) =
    val (s1, b) = f(init, value)
    val v       = variation.map(_.mapAccuml(init)(f)._2)
    child.map(_.mapAccuml(s1)(f)) match
      case None    => (s1, Node(b, None, v))
      case Some(s) => (s._1, Node(b, s._2.some, v))

  final def mapAccuml_[S, B](init: S)(f: (S, A) => (S, B)): Node[B] =
    mapAccuml(init)(f)._2

  // Akin to mapAccuml, return an Option[Node[B]]
  // when a node from mainline returns None, we stop traverse down that line
  // when a variation node returns None, we just ignore it and continue to traverse next variations
  // TODO: now if the f(value) is None, the whole tree is None
  // should we promote a variation to mainline if the f(value) is None?
  final def _mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): (S, Option[Node[B]]) =
    f(init, value) match
      case (s1, None) => (s1, None)
      case (s1, Some(b)) =>
        val vs = variation.map(_._mapAccumlOption(init)(f)._2).flatten
        child.map(_._mapAccumlOption(s1)(f)) match
          case None    => (s1, Node(b, None, vs).some)
          case Some(s) => (s._1, Node(b, s._2, vs).some)

  final def mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): Option[Node[B]] =
    _mapAccumlOption(init)(f)._2

  // returns child if it sastisfies the predicate
  final def getChild(predicate: A => Boolean): Option[Node[A]] =
    child.flatMap(c => if predicate(c.value) then c.some else None)

  final def withChild(child: Node[A]): Node[A] =
    copy(child = child.some)

  final def withoutChild: Node[A] =
    copy(child = None)

  def variationsWithIndex: List[(Node[A], Int)] =
    variation.fold(Nil)(_.variations.zipWithIndex)

  // find the first variation that statisfies the predicate
  final def findVariation(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else
      variation.fold(none[Node[A]]): v =>
        if predicate(v.value) then v.some
        else v.findVariation(predicate)

  // find the first variation byId
  final def findVariation[Id](id: Id)(using h: HasId[A, Id]): Option[Node[A]] =
    findVariation(h.getId(_) == id)

  // check if exist a variation that statisfies the predicate
  final def hasVariation(predicate: A => Boolean): Boolean =
    findVariation(predicate).isDefined

  // check if exist a variation that has Id
  final def hasVariation[Id](id: Id)(using h: HasId[A, Id]): Boolean =
    hasVariation(h.getId(_) == id)

  final def withVariations(variation: Node[A]): Node[A] =
    copy(variation = variation.some)

  final def withoutVariations: Node[A] =
    copy(variation = None)

  // find node in the mainline
  final def findInMainline(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else
      child.fold(none[Node[A]]): c =>
        if predicate(c.value) then c.some
        else c.findInMainline(predicate)

  final def lastMainlineNode: Node[A] =
    child.fold(this)(_.lastMainlineNode)

  final def modifyLastMainlineNode(f: Node[A] => Node[A]): Node[A] =
    child.fold(f(this))(c => withChild(c.modifyLastMainlineNode(f)))

  // final def findChildOrVariation(predicate: A => Boolean): Option[Node[A]] =
  //   childAndVariations.foldLeft(none[Node[A]]): (acc, v) =>
  //     if acc.isDefined then acc
  //     else if predicate(v.value) then v.some
  //     else None

  // find the first Node that statisfies the predicate
  final def findNode(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else childAndVariations.foldLeft(none[Node[A]])((b, n) => b.orElse(n.findNode(predicate)))

  // add new variation at the end of
  // final def addVariation(node: Option[Node[A]])(using s: Semigroup[A]): Node[A] =
  // copy(variation = variation.mergeVariations(variation.some))

  final def replaceNode(predicate: A => Boolean)(node: Node[A]): Option[Node[A]] =
    modifyNode(predicate)(_ => node)

  // modify the first node that satisfies the predicate (dfs with main line and then variations)
  final def modifyNode(predicate: A => Boolean)(f: Node[A] => Node[A]): Option[Node[A]] =
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
  final def deleteSubNode(predicate: A => Boolean): Option[Node[A]] =
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
