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

case class PgnNodeData(san: San, metas: Metas)

case class VariationNode[A, B](node: Node[A, B], extra: B)

type PgnTree = Node[PgnNodeData, List[Comment]]

// isomorphic to ParsedPgn
case class NewPgn(initialPosition: InitialPosition, tags: Tags, tree: Option[PgnTree]):
  def toParsedPgn: ParsedPgn =
    val sans = tree.fold(List.empty[San])(traverse)
    ParsedPgn(initialPosition, tags, Sans(sans))

  def traverse(node: PgnTree): List[San] =
    val variations = node.variations.map(x => Variation(x.extra, Sans(traverse(x.node))))
    val san        = node.move.san.withMetas(node.move.metas).withVariations(variations)
    san :: node.child.fold(Nil)(traverse)

object NewPgn:
  extension (san: San) def clean: San = san.withMetas(Metas.empty).withVariations(Nil)

  extension (pgn: ParsedPgn)
    def toNewPgn: NewPgn =
      val tree = pgn.sans.value.reverse.foldLeft(none[PgnTree]) { (o, san) =>
        Some(toNode(san, o))
      }
      NewPgn(initialPosition = pgn.initialPosition, tags = pgn.tags, tree = tree)

  def toNode(san: San, child: Option[PgnTree]): PgnTree =
    Node(
      PgnNodeData(san.clean, san.metas),
      child,
      san.metas.variations.flatMap(v => toVariationNode(v.sans, v.comments))
    )

  def toVariationNode(sans: Sans, comments: List[Comment]) =
    val node = sans.value.reverse.foldLeft(none[PgnTree]) { (o, san) =>
      o match
        case None       => Some(toNode(san, None))
        case Some(node) => Some(toNode(san, Some(node)))
    }
    node.map(VariationNode(_, comments))
