package chess
package format
package pgn

import cats.syntax.all.*
import cats.derived.*
import cats.*

import monocle.Traversal

// question: what if a variation without moves?
// answer: it's possible but we need to support null move first
case class Node[A](
    value: A,
    child: Option[Node[A]],
    variations: List[Node[A]]
):
  def mainLine: List[A] = value :: child.fold(Nil)(_.mainLine)
  def totalNodes: Int   = this.foldLeft(0)((b, a) => b + 1)

  // search and replace a node that satisfies the predicate (both child and variations)
  def setChild(predicate: A => Boolean)(node: Node[A]): Node[A] =
    if predicate(value) then copy(child = Some(node))
    else copy(child = child.map(_.setChild(predicate)(node)))

  def removeChild(predicate: A => Boolean): Node[A] =
    if predicate(value) then copy(child = None)
    else copy(child = child.map(_.removeChild(predicate)))

  def modifyChild(predicate: A => Boolean)(f: A => A): Node[A] = ???

  def setVariations(predicate: A => Boolean)(variations: List[Node[A]]): Node[A]            = ???
  def modifyVariations(predicate: A => Boolean)(f: List[Node[A]] => List[Node[A]]): Node[A] = ???
  def revalueVariations(predicate: A => Boolean): Node[A]                                   = ???

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
      val b1 = fa.variations.foldRight(lb)((a, b) => a.foldRight(b)(f))
      val b2 = fa.child.foldRight(b1)((a, b) => foldRight(a, b)(f))
      f(fa.value, b2)

  def filterKey[A](predicate: A => Boolean): Traversal[Node[A], A] = new:
    def modifyA[F[_]: Applicative](f: A => F[A])(s: Node[A]): F[Node[A]] =
      s.map(a => if predicate(a) then f(a) else a.pure[F]).sequence

type PgnTree = Node[Move]

// isomorphic to Pgn
case class NewPgn(tags: Tags, initial: Initial, tree: Option[PgnTree]):
  def toPgn: Pgn =
    val moves = tree.fold(List.empty[Move])(toMove(_, Ply(1)))
    val turns = Turn.fromMoves(moves, Ply(1))
    Pgn(tags, turns, initial)

  def toMove(node: PgnTree, ply: Ply): List[Move] =
    val variations = node.variations.map(x => Turn.fromMoves(toMove(x, ply), ply))
    val move       = node.value.copy(variations = variations)
    move :: node.child.fold(Nil)(toMove(_, ply + 1))

object NewPgn:
  def moves(turn: Turn): List[Move] = List(turn.white, turn.black).flatten
  def moves(pgn: Pgn): List[Move]   = pgn.turns.flatMap(moves)

  extension (move: Move) def clean: Move = move.copy(variations = Nil)
  def apply(pgn: Pgn): NewPgn =
    val tree = moves(pgn).reverse.foldLeft(none[PgnTree]) { (o, move) => Some(toNode(move, o)) }
    NewPgn(tags = pgn.tags, initial = pgn.initial, tree = tree)

  def toNode(move: Move, child: Option[PgnTree]): PgnTree =
    Node(
      move.clean,
      child,
      move.variations.map(_.flatMap(moves)).map(x => toNode(x.head, None))
    )
