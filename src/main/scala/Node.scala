package chess

import cats.*
import cats.derived.*
import cats.syntax.all.*
import scala.annotation.tailrec

sealed abstract class Tree[A](val value: A, val child: Option[Node[A]]) derives Functor, Traverse:

  def withValue(value: A): IsTree[A, this.type] = this match
    case n: Node[A]      => n.copy(value = value)
    case v: Variation[A] => v.copy(value = value)

  def withChild(child: Node[A]): IsTree[A, this.type] =
    this match
      case n: Node[A]      => n.copy(child = child.some)
      case v: Variation[A] => v.copy(child = child.some)

  def withoutChild: IsTree[A, this.type] =
    this match
      case n: Node[A]      => n.copy(child = None)
      case v: Variation[A] => v.copy(child = None)

  def mainline: List[Tree[A]] = this :: child.fold(List.empty[Tree[A]])(_.mainline)
  def mainlineValues: List[A] = value :: child.fold(List.empty[A])(_.mainlineValues)

  def hasId[Id](id: Id)(using h: HasId[A, Id]): Boolean = h.getId(value) == id

  def findPath[Id](path: List[Id])(using h: HasId[A, Id]): Option[List[Tree[A]]]

  def find[Id](path: List[Id])(using h: HasId[A, Id]): Option[Tree[A]] =
    findPath(path).flatMap(_.lastOption)

  // find a node with path
  // if not found, return None
  // if found node has no child, add new value as a child
  // if found node has a child, add new value as it's child's variation
  def addNodeAt[Id](path: List[Id])(node: A)(using h: HasId[A, Id]): Option[Tree[A]] =
    val f: TreeModifier[A] = (tree: Tree[A]) =>
      val child =
        tree.child.fold(Node(node))(child => child.withVariations(child.variations :+ Variation(node)))
      tree.withChild(child)
    modifyAt(path, f)

  def modifyAt[Id](path: List[Id], f: TreeModifier[A])(using h: HasId[A, Id]): Option[Tree[A]]

object Tree:
  def lift[A](f: A => A): TreeModifier[A] = tree => tree.withValue(f(tree.value))

type IsTree[A, X <: Tree[A]] = X match
  case Node[A]      => Node[A]
  case Variation[A] => Variation[A]

type TreeModifier[A] = (tree: Tree[A]) => IsTree[A, tree.type]

final case class Node[A](
    override val value: A,
    override val child: Option[Node[A]] = None,
    variations: List[Variation[A]] = Nil
) extends Tree[A](value, child)
    derives Functor,
      Traverse:

  def findPath[Id](path: List[Id])(using h: HasId[A, Id]): Option[List[Tree[A]]] =
    @tailrec
    def loop(node: Node[A], path: List[Id], acc: List[Tree[A]]): Option[List[Tree[A]]] =
      path match
        case Nil => acc.some
        case head :: Nil if node.hasId(head) =>
          (node :: acc).some
        case head :: rest if node.hasId(head) =>
          node.child match
            case Some(child) => loop(child, rest, node :: acc)
            case None        => None
        case _ =>
          node.findVariation(path.head) match
            case Some(variation) => loop(variation.toNode, path, acc)
            case None            => None

    if path.isEmpty then None else loop(this, path, Nil).map(_.reverse)

  def modifyAt[Id](path: List[Id], f: TreeModifier[A])(using h: HasId[A, Id]): Option[Node[A]] =
    path match
      case Nil                        => None
      case head :: Nil if hasId(head) => f(this).some
      case head :: rest if hasId(head) =>
        child.flatMap(_.modifyAt(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ =>
        variations.foldLeft((false, List.empty[Variation[A]])) {
          case ((true, acc), n) => (true, acc :+ n)
          case ((false, acc), n) =>
            n.modifyAt(path, f) match
              case Some(nn) => (true, acc :+ nn)
              case None     => (false, acc :+ n)
        } match
          case (true, ns) => copy(variations = ns).some
          case (false, _) => none

  // Akin to map, but allows to keep track of a state value when calling the function.
  def mapAccuml[S, B](init: S)(f: (S, A) => (S, B)): (S, Node[B]) =
    val (s1, b) = f(init, value)
    val v       = variations.map(_.mapAccuml(init)(f)._2)
    child.map(_.mapAccuml(s1)(f)) match
      case None    => (s1, Node(b, None, v))
      case Some(s) => (s._1, Node(b, s._2.some, v))

  def mapAccuml_[S, B](init: S)(f: (S, A) => (S, B)): Node[B] =
    mapAccuml(init)(f)._2

  // Akin to mapAccuml, return an Option[Node[B]]
  // when a node from mainline returns None, we stop traverse down that line
  // when a variation node returns None, we just ignore it and continue to traverse next variations
  // TODO: now if the f(value) is None, the whole tree is None
  // should we promote a variation to mainline if the f(value) is None?
  def _mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): (S, Option[Node[B]]) =
    f(init, value) match
      case (s1, None) => (s1, None)
      case (s1, Some(b)) =>
        val vs = variations.map(_._mapAccumlOption(init)(f)._2).flatten
        child.map(_._mapAccumlOption(s1)(f)) match
          case None    => (s1, Node(b, None, vs).some)
          case Some(s) => (s._1, Node(b, s._2, vs).some)

  def mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): Option[Node[B]] =
    _mapAccumlOption(init)(f)._2

  // find node in the mainline
  def findInMainline(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else
      child.fold(none[Node[A]]): c =>
        if predicate(c.value) then c.some
        else c.findInMainline(predicate)

  def modifyInMainline(predicate: A => Boolean, f: Node[A] => Node[A]): Option[Node[A]] =
    if predicate(value) then f(this).some
    else
      child.flatMap(_.modifyInMainline(predicate, f)) match
        case Some(c) => withChild(c).some
        case None    => None

  def lastMainlineNode: Node[A] =
    child.fold(this)(_.lastMainlineNode)

  def modifyLastMainlineNode(f: Node[A] => Node[A]): Node[A] =
    child.fold(f(this))(c => withChild(c.modifyLastMainlineNode(f)))

  // map values from mainline
  // remove all variations
  def mapMainline[B](f: A => B): Node[B] =
    copy(value = f(value), child = child.map(_.mapMainline(f)), variations = Nil)

  def mapMainlineWithIndex[B](f: (A, Int) => B): Node[B] = ???

  def toVariation: Variation[A] = Variation(value, child)

  def withVariations(variations: List[Variation[A]]): Node[A] =
    copy(variations = variations)

  def withoutVariations: Node[A] =
    copy(variations = Nil)

  // find the first variation by id
  def findVariation[Id](id: Id)(using h: HasId[A, Id]): Option[Variation[A]] =
    variations.find(_.hasId(id))

  // check if exist a variation that has Id
  def hasVariation[Id](id: Id)(using h: HasId[A, Id]): Boolean =
    variations.exists(_.hasId(id))

object Node:
  def lift[A](f: A => A): Node[A] => Node[A] = tree => tree.withValue(f(tree.value))

final case class Variation[A](override val value: A, override val child: Option[Node[A]] = None)
    extends Tree[A](value, child) derives Functor, Traverse:

  def findPath[Id](path: List[Id])(using h: HasId[A, Id]): Option[List[Tree[A]]] =
    toNode.findPath(path)

  def modifyAt[Id](path: List[Id], f: TreeModifier[A])(using h: HasId[A, Id]): Option[Variation[A]] =
    path match
      case Nil                        => None
      case head :: Nil if hasId(head) => f(this).some
      case head :: rest if hasId(head) =>
        child.flatMap(_.modifyAt(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ => None

  // Akin to map, but allows to keep track of a state value when calling the function.
  def mapAccuml[S, B](init: S)(f: (S, A) => (S, B)): (S, Variation[B]) =
    val (s1, b) = f(init, value)
    child.map(_.mapAccuml(s1)(f)) match
      case None    => (s1, Variation(b, None))
      case Some(s) => (s._1, Variation(b, s._2.some))

  // Akin to mapAccuml, return an Option[Node[B]]
  // when a node from mainline returns None, we stop traverse down that line
  // when a variation node returns None, we just ignore it and continue to traverse next variations
  // TODO: now if the f(value) is None, the whole tree is None
  // should we promote a variation to mainline if the f(value) is None?
  def _mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): (S, Option[Variation[B]]) =
    f(init, value) match
      case (s1, None) => (s1, None)
      case (s1, Some(b)) =>
        child.map(_._mapAccumlOption(s1)(f)) match
          case None    => (s1, Variation(b, None).some)
          case Some(s) => (s._1, Variation(b, s._2).some)

  def toNode: Node[A] = Node(value, child)
