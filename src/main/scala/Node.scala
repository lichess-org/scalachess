package chess

import cats.*
import cats.derived.*
import cats.syntax.all.*
import scala.annotation.tailrec

sealed abstract class Tree[A](val value: A, val child: Option[Node[A]]) derives Functor, Traverse:

  final def withValue(value: A): TreeSelector[A, this.type] = this match
    case n: Node[A]      => n.copy(value = value)
    case v: Variation[A] => v.copy(value = value)

  final def withChild(child: Node[A]): TreeSelector[A, this.type] =
    this match
      case n: Node[A]      => n.copy(child = child.some)
      case v: Variation[A] => v.copy(child = child.some)

  final def setChild(child: Option[Node[A]]): TreeSelector[A, this.type] =
    this match
      case n: Node[A]      => n.copy(child = child)
      case v: Variation[A] => v.copy(child = child)

  final def withoutChild: TreeSelector[A, this.type] =
    this match
      case n: Node[A]      => n.copy(child = None)
      case v: Variation[A] => v.copy(child = None)

  final def mainline: List[Tree[A]] = this :: child.fold(List.empty[Tree[A]])(_.mainline)
  final def mainlineValues: List[A] = value :: child.fold(List.empty[A])(_.mainlineValues)

  final def hasId[Id](id: Id)(using HasId[A, Id]): Boolean = value.hasId(id)

  final def findPath[Id](path: List[Id])(using HasId[A, Id]): Option[List[Tree[A]]] =
    @tailrec
    def loop(tree: Tree[A], path: List[Id], acc: List[Tree[A]]): Option[List[Tree[A]]] =
      path match
        case Nil => acc.some
        case head :: Nil if tree.hasId(head) =>
          (tree :: acc).some
        case head :: rest if tree.hasId(head) =>
          tree.child match
            case Some(child) => loop(child, rest, tree :: acc)
            case None        => None
        case _ =>
          tree match
            case node: Node[A] =>
              node.findVariation(path.head) match
                case Some(variation) => loop(variation.toNode, path, acc)
                case None            => None
            case _ => None

    if path.isEmpty then None else loop(this, path, Nil).map(_.reverse)

  final def find[Id](path: List[Id])(using HasId[A, Id]): Option[Tree[A]] =
    findPath(path).flatMap(_.lastOption)

  final def pathExists[Id](path: List[Id]): HasId[A, Id] ?=> Boolean =
    findPath(path).isDefined

  def modifyAt[Id](path: List[Id], f: TreeMapper[A])(using HasId[A, Id]): Option[Tree[A]]
  def modifyWithParentPath[Id](path: List[Id], f: Node[A] => Node[A])(using HasId[A, Id]): Option[Tree[A]]
  def deleteAt[Id](path: List[Id])(using HasId[A, Id]): Option[Option[Tree[A]]]

  private def self: TreeSelector[A, this.type] = this match
    case n: Node[A]      => n
    case v: Variation[A] => v

object Tree:
  def lift[A](f: A => A): TreeMapper[A] = tree => tree.withValue(f(tree.value))

  // Add a value as a child or variation
  // if the tree has no child, add value as child
  // if the value has the same id as the child, merge the values
  // otherwise add value as a variation (and merge it to one of the existing variation if necessary)
  def addValueAsChildOrVariation[A, Id]: HasId[A, Id] ?=> Mergeable[A] ?=> A => TreeMapper[A] = value =>
    tree => addChildOrVariation(Node(value))(tree)

  def addChildOrVariation[A, Id]: HasId[A, Id] ?=> Mergeable[A] ?=> Node[A] => TreeMapper[A] = other =>
    tree =>
      val child = tree.child.fold(other)(_.merge(other))
      tree.withChild(child)

  extension [A](vs: List[Variation[A]])
    // add a variation to the list of variations
    // if there is already a variation with the same id, merge the values
    def add[Id](v: Variation[A])(using HasId[A, Id], Mergeable[A]): List[Variation[A]] =
      @tailrec
      def loop(acc: List[Variation[A]], rest: List[Variation[A]])(using HasId[A, Id]): List[Variation[A]] =
        rest match
          case Nil => acc :+ v
          case x :: xs =>
            if x.value.sameId(v.value) then (acc ++ (x.withValue(x.value.merge(v.value)) +: xs))
            else loop(acc :+ x, xs)
      loop(Nil, vs)

    def add[Id](xs: List[Variation[A]])(using HasId[A, Id], Mergeable[A]): List[Variation[A]] =
      xs.foldLeft(vs)((acc, x) => acc.add(x))

type TreeSelector[A, X <: Tree[A]] = X match
  case Node[A]      => Node[A]
  case Variation[A] => Variation[A]

type TreeMapper[A] = (tree: Tree[A]) => TreeSelector[A, tree.type]

final case class Node[A](
    override val value: A,
    override val child: Option[Node[A]] = None,
    variations: List[Variation[A]] = Nil
) extends Tree[A](value, child)
    derives Functor,
      Traverse:

  // take the first n nodes in the mainline
  // keep all variations
  // n > 0
  def take(n: Int): Node[A] =
    @tailrec
    def loop(n: Int, node: Node[A], acc: List[Node[A]]): List[Node[A]] =
      if n <= 0 then acc
      else
        node.child match
          case None        => node :: acc
          case Some(child) => loop(n - 1, child, node.withoutChild :: acc)
    if n == 0 then this
    else loop(n, this, Nil).foldLeft(none[Node[A]])((acc, node) => node.setChild(acc).some).getOrElse(this)

  // get the nth node of in the mainline
  def apply(n: Int): Option[Node[A]] =
    @tailrec
    def loop(tree: Option[Node[A]], n: Int): Option[Node[A]] =
      if n <= 0 then tree
      else loop(tree.flatMap(_.child), n - 1)
    loop(this.some, n)

  def mainlinePath[Id](using HasId[A, Id]): List[Id] =
    value.id +: child.fold(List.empty[Id])(_.mainlinePath)

  def modifyWithParentPath[Id](path: List[Id], f: Node[A] => Node[A])(using HasId[A, Id]): Option[Node[A]] =
    path match
      case Nil                        => f(this).some
      case head :: Nil if hasId(head) => child.map(c => withChild(f(c)))
      case head :: rest if hasId(head) =>
        child.flatMap(_.modifyWithParentPath(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ =>
        variations.foldLeft((false, List.empty[Variation[A]])) {
          case ((true, acc), n) => (true, acc :+ n)
          case ((false, acc), n) =>
            n.modifyWithParentPath(path, f) match
              case Some(nn) => (true, acc :+ nn)
              case None     => (false, acc :+ n)
        } match
          case (true, ns) => copy(variations = ns).some
          case (false, _) => none

  def modifyAt[Id](path: List[Id], f: TreeMapper[A])(using HasId[A, Id]): Option[Node[A]] =
    path match
      case Nil                        => None
      case head :: Nil if hasId(head) => f(this).some
      case head :: rest if hasId(head) =>
        child.flatMap(_.modifyAt(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ =>
        variations.foldLeft((false, List.empty[Variation[A]])) {
          case ((true, acc), n) => (true, n :: acc)
          case ((false, acc), n) =>
            n.modifyAt(path, f) match
              case Some(nn) => (true, nn :: acc)
              case None     => (false, n :: acc)
        } match
          case (true, ns) => copy(variations = ns.reverse).some
          case (false, _) => none

  // delete the node at the end of the path
  // return None if path is not found
  // return Some(None) if the node is deleted
  def deleteAt[Id](path: List[Id])(using HasId[A, Id]): Option[Option[Node[A]]] =
    path match
      case Nil                        => None
      case head :: Nil if hasId(head) => Some(None)
      case head :: rest if hasId(head) =>
        child.flatMap(_.deleteAt(rest)) match
          case None    => None
          case Some(c) => copy(child = c).some.some
      case _ =>
        variations.foldLeft((false, List.empty[Variation[A]])) {
          case ((true, acc), n) => (true, n :: acc)
          case ((false, acc), n) =>
            n.deleteAt(path) match
              case Some(nn) => (true, nn ++: acc)
              case None     => (false, n :: acc)
        } match
          case (true, ns) => copy(variations = ns.reverse).some.some
          case (false, _) => none

  // find a node with path
  // if not found, return None
  // if found node has no child, add new value as a child
  // if found node has a child, add new value as it's child's variation
  def addValueAsChildOrVariationAt[Id](path: List[Id], value: A)(using
      HasId[A, Id],
      Mergeable[A]
  ): Option[Node[A]] =
    modifyAt(path, Tree.addValueAsChildOrVariation(value))

  // add a node with path to the tree
  // which basically is addChildOrVariationAt with path +: h.getId(value)
  def addValueAt[Id](path: List[Id])(value: A)(using HasId[A, Id], Mergeable[A]): Option[Node[A]] =
    addNodeAt(path)(Node(value))

  def addNodeAt[Id](path: List[Id])(other: Node[A])(using HasId[A, Id], Mergeable[A]): Option[Node[A]] =
    modifyWithParentPath(path, _.merge(other))

  // Akin to map, but allows to keep track of a state value when calling the function.
  // This is different from Traverse.mapAccumulate
  // because variations only use accumulated value from the parent
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
  def mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): (S, Option[Node[B]]) =
    f(init, value) match
      case (s1, None) => (s1, None)
      case (s1, Some(b)) =>
        val vs = variations.map(_.mapAccumlOption(init)(f)._2).flatten
        child.map(_.mapAccumlOption(s1)(f)) match
          case None    => (s1, Node(b, None, vs).some)
          case Some(s) => (s._1, Node(b, s._2, vs).some)

  def mapAccumlOption_[S, B](init: S)(f: (S, A) => (S, Option[B])): Option[Node[B]] =
    mapAccumlOption(init)(f)._2

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

  // akin to mapMainline, but also pass index to the function
  def mapMainlineWithIndex[B](f: (A, Int) => B): Node[B] =
    def loop(n: Node[A], i: Int): Node[B] =
      n.copy(value = f(n.value, i), child = n.child.map(c => loop(c, i + 1)), variations = Nil)
    loop(this, 0)

  def toVariation: Variation[A]        = Variation(value, child)
  def toVariations: List[Variation[A]] = Variation(value, child) +: variations

  // merge two nodes
  // in case of same id, merge values
  // else add as variation
  def merge[Id](other: Node[A])(using HasId[A, Id], Mergeable[A]): Node[A] =
    if value.sameId(value) then withValue(value.merge(value)).withVariations(variations.add(other.variations))
    else withVariations(variations.add(other.toVariations))

  def addVariation[Id](v: Variation[A])(using HasId[A, Id], Mergeable[A]): Node[A] =
    withVariations(variations.add(v))

  def addVariations[Id](vs: List[Variation[A]])(using HasId[A, Id], Mergeable[A]): Node[A] =
    withVariations(variations.add(vs))

  def withVariations(variations: List[Variation[A]]): Node[A] =
    copy(variations = variations)

  def withoutVariations: Node[A] =
    copy(variations = Nil)

  // find the first variation by id
  def findVariation[Id](id: Id)(using HasId[A, Id]): Option[Variation[A]] =
    variations.find(_.hasId(id))

  // check if exist a variation that has Id
  def hasVariation[Id](id: Id)(using HasId[A, Id]): Boolean =
    variations.exists(_.hasId(id))

  def withoutChildAndVariations: Node[A] =
    copy(child = None, variations = Nil)

object Node:
  def lift[A](f: A => A): Node[A] => Node[A] = tree => tree.withValue(f(tree.value))

  def build[A, B](s: Seq[A], f: A => B): Option[Node[B]] =
    s.reverse.foldLeft(none[Node[B]])((acc, a) => Node(f(a), acc).some)

  def buildWithIndex[A, B](s: Seq[A], f: (A, Int) => B): Option[Node[B]] =
    build(s.zipWithIndex, f.tupled)

  def buildWithNode[A, B](s: Seq[A], f: A => Node[B]): Option[Node[B]] =
    s.reverse match
      case Nil     => none[Node[B]]
      case x :: xs => xs.foldLeft(f(x))((acc, a) => f(a).withChild(acc)).some

final case class Variation[A](override val value: A, override val child: Option[Node[A]] = None)
    extends Tree[A](value, child) derives Functor, Traverse:

  def modifyWithParentPath[Id](path: List[Id], f: Node[A] => Node[A])(using
      HasId[A, Id]
  ): Option[Variation[A]] =
    path match
      case Nil                        => None
      case head :: Nil if hasId(head) => child.map(c => withChild(f(c)))
      case head :: rest if hasId(head) =>
        child.flatMap(_.modifyWithParentPath(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ => None

  def modifyAt[Id](path: List[Id], f: TreeMapper[A])(using HasId[A, Id]): Option[Variation[A]] =
    path match
      case Nil                        => None
      case head :: Nil if hasId(head) => f(this).some
      case head :: rest if hasId(head) =>
        child.flatMap(_.modifyAt(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ => None

  // delete the node at the end of the path
  // return None if path is not found
  // return Some(None) if the node is deleted
  def deleteAt[Id](path: List[Id])(using HasId[A, Id]): Option[Option[Variation[A]]] =
    path match
      case Nil                        => None
      case head :: Nil if hasId(head) => Some(None)
      case head :: rest if hasId(head) =>
        child.flatMap(_.deleteAt(rest)) match
          case None    => None
          case Some(c) => copy(child = c).some.some
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
  def mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): (S, Option[Variation[B]]) =
    f(init, value) match
      case (s1, None) => (s1, None)
      case (s1, Some(b)) =>
        child.map(_.mapAccumlOption(s1)(f)) match
          case None    => (s1, Variation(b, None).some)
          case Some(s) => (s._1, Variation(b, s._2).some)

  def toNode: Node[A] = Node(value, child)

object Variation:

  def build[A, B](s: Seq[A], f: A => B): Option[Variation[B]] =
    Node.build(s, f).map(_.toVariation)

  def buildWithIndex[A, B](s: Seq[A], f: (A, Int) => B): Option[Variation[B]] =
    Node.buildWithIndex(s, f).map(_.toVariation)
