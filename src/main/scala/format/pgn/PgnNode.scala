package chess
package format
package pgn

import cats.syntax.all.*

case class Node[A](
    move: A,
    child: Option[Node[A]],
    variations: List[Node[A]]
):
  def mainLine: List[A] = move :: child.fold(Nil)(_.mainLine)

case class PgnNodeData(san: San, metas: Metas, variationComments: Option[List[Comment]])
type ParsedPgnTree = Node[PgnNodeData]

// isomorphic to ParsedPgn
case class NewParsedPgn(initialPosition: InitialPosition, tags: Tags, tree: Option[ParsedPgnTree]):
  def toParsedPgn: ParsedPgn =
    val sans = tree.fold(List.empty[San])(toSan)
    ParsedPgn(initialPosition, tags, Sans(sans))

  def toSan(node: ParsedPgnTree): List[San] =
    val variations = node.variations.map(toVariation)
    val san        = node.move.san.withMetas(node.move.metas).withVariations(variations)
    san :: node.child.fold(Nil)(toSan)

  def toVariation(node: ParsedPgnTree): Variation =
    val comments = node.move.variationComments.getOrElse(Nil)
    val sans     = toSan(node)
    Variation(comments, Sans(sans))

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
      PgnNodeData(san.clean, san.metas, None),
      child,
      san.metas.variations.flatMap(v => toVariationNode(v.sans, v.comments))
    )

  def toVariationNode(sans: Sans, comments: List[Comment]) =
    sans.value.reverse
      .foldLeft(none[ParsedPgnTree])((o, san) => Some(toNode(san, o)))
      .map(x => x.copy(move = x.move.copy(variationComments = comments.some)))

type PgnTree = Node[Move]
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
  def toNewPgn(pgn: Pgn): NewPgn =
    val tree = moves(pgn).reverse.foldLeft(none[PgnTree]) { (o, move) => Some(toNode(move, o)) }
    NewPgn(tags = pgn.tags, initial = pgn.initial, tree = tree)

  def toNode(move: Move, child: Option[PgnTree]): PgnTree =
    Node(
      move.clean,
      child,
      move.variations.map(_.flatMap(moves)).map(x => toNode(x.head, None))
    )
