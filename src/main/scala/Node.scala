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

  def toNode: Node[A] = Node(value, child)
