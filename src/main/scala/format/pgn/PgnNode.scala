package chess
package format
package pgn

import cats.syntax.all.*
import cats.derived.*
import cats.Functor
import cats.Traverse

// other idea?
enum Tree[+A] derives Functor, Traverse:
  case Node(move: A, child: Tree[A], variations: List[Tree[A]])
  case Empty

// question: what if a variation without moves?
// answer: it's possible but we need to support null move first
case class PgnNode[A](
    move: A,
    child: Option[PgnNode[A]],
    variations: List[PgnNode[A]]
) derives Functor,
      Traverse:
  def mainLine: List[A] = move :: child.fold(Nil)(_.mainLine)
  def totalNodes: Int   = this.foldLeft(0)((b, a) => b + 1)

case class PgnNodeData(san: San, metas: Metas, variationComments: Option[List[Comment]])
type ParsedPgnTree = PgnNode[PgnNodeData]

// isomorphic to ParsedPgn
case class NewParsedPgn(initialPosition: InitialPosition, tags: Tags, tree: Option[ParsedPgnTree]):
  def mainLine = tree.fold(List.empty[San])(_.mainLine.map(_.san))

type PgnTree = PgnNode[Move]

// isomorphic to Pgn
case class NewPgn(tags: Tags, initial: Initial, tree: Option[PgnTree]):
  def toPgn: Pgn =
    val moves = tree.fold(List.empty[Move])(toMove(_, Ply(1)))
    val turns = Turn.fromMoves(moves, Ply(1))
    Pgn(tags, turns, initial)

  def toMove(node: PgnTree, ply: Ply): List[Move] =
    val variations = node.variations.map(x => Turn.fromMoves(toMove(x, ply), ply))
    val move       = node.move.copy(variations = variations)
    move :: node.child.fold(Nil)(toMove(_, ply + 1))

object NewPgn:
  def moves(turn: Turn): List[Move] = List(turn.white, turn.black).flatten
  def moves(pgn: Pgn): List[Move]   = pgn.turns.flatMap(moves)

  extension (move: Move) def clean: Move = move.copy(variations = Nil)
  def apply(pgn: Pgn): NewPgn =
    val tree = moves(pgn).reverse.foldLeft(none[PgnTree]) { (o, move) => Some(toNode(move, o)) }
    NewPgn(tags = pgn.tags, initial = pgn.initial, tree = tree)

  def toNode(move: Move, child: Option[PgnTree]): PgnTree =
    PgnNode(
      move.clean,
      child,
      move.variations.map(_.flatMap(moves)).map(x => toNode(x.head, None))
    )
