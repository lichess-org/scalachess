package chess
package format
package pgn

import cats.syntax.all.*

case class Node[A, B](
    move: A,
    child: Option[Node[A, B]],
    variations: List[VariationNode[A, B]]
):
  def mainLine: List[A] = move :: child.fold(List.empty[A])(_.mainLine)

case class VariationNode[A, B](node: Node[A, B], extra: B)

case class PgnNodeData(san: San, metas: Metas)
type ParsedPgnTree = Node[PgnNodeData, List[Comment]]

// isomorphic to ParsedPgn
case class NewParsedPgn(initialPosition: InitialPosition, tags: Tags, tree: Option[ParsedPgnTree]):
  def toParsedPgn: ParsedPgn =
    val sans = tree.fold(List.empty[San])(toSan)
    ParsedPgn(initialPosition, tags, Sans(sans))

  def toSan(node: ParsedPgnTree): List[San] =
    val variations = node.variations.map(x => Variation(x.extra, Sans(toSan(x.node))))
    val san        = node.move.san.withMetas(node.move.metas).withVariations(variations)
    san :: node.child.fold(Nil)(toSan)

object NewParsedPgn:
  extension (san: San) def clean: San = san.withMetas(Metas.empty).withVariations(Nil)

  extension (pgn: ParsedPgn)
    def toNewPgn: NewParsedPgn =
      val tree = pgn.sans.value.reverse.foldLeft(none[ParsedPgnTree]) { (o, san) =>
        Some(toNode(san, o))
      }
      NewParsedPgn(initialPosition = pgn.initialPosition, tags = pgn.tags, tree = tree)

  def toNode(san: San, child: Option[ParsedPgnTree]): ParsedPgnTree =
    Node(
      PgnNodeData(san.clean, san.metas),
      child,
      san.metas.variations.flatMap(v => toVariationNode(v.sans, v.comments))
    )

  def toVariationNode(sans: Sans, comments: List[Comment]) =
    val node = sans.value.reverse.foldLeft(none[ParsedPgnTree]) { (o, san) =>
      o match
        case None       => Some(toNode(san, None))
        case Some(node) => Some(toNode(san, Some(node)))
    }
    node.map(VariationNode(_, comments))

type PgnTree = Node[Move, Unit]
// isomorphic to Pgn
case class NewPgn(tags: Tags, initial: Initial, tree: Option[PgnTree]):
  def toPgn: Pgn =
    val moves = tree.fold(List.empty[Move])(toMove(_, Ply(1)))
    val turns = Turn.fromMoves(moves, Ply(1))
    Pgn(tags, turns, initial)

  def toMove(node: PgnTree, ply: Ply): List[Move] =
    val variations = node.variations.map(x => Turn.fromMoves(toMove(x.node, ply), ply))
    val move       = node.move.copy(variations = variations)
    move :: node.child.fold(Nil)(toMove(_, ply + 1))

object NewPgn:
  def moves(turn: Turn): List[Move] = List(turn.white, turn.black).flatten
  def moves(pgn: Pgn): List[Move]   = pgn.turns.flatMap(moves)

  extension (move: Move) def clean: Move = move.copy(variations = Nil)
  def toNewPgn(pgn: Pgn): NewPgn =
    val tree = moves(pgn).reverse.foldLeft(none[PgnTree]) { (o, move) => Some(toNode(move, o)) }
    NewPgn(tags = pgn.tags, initial = pgn.initial, tree = tree)

  def toNode(move: Move, child: Option[PgnTree]): PgnTree =
    Node(
      move.clean,
      child,
      move.variations.map(_.flatMap(moves)).map(x => VariationNode(toNode(x.head, None), ()))
    )
