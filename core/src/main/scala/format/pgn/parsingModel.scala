package chess
package format.pgn

import cats.syntax.all.*
import chess.Position.AndFullMoveNumber

// We don't support variation without move now,
// but we can in the future when we support null move
case class PgnNodeData(
    san: San,
    metas: Metas, // describes the position after the move `san` is played
    /** `variationComments` are comments before the first move of a variation. Example:
     * `1.d4 ( { on the other hand } 1.e4  )`
     * => PgnNodeData(1.d4, Metas.empty, List(Node(1.e4, metas = Metas.empty, variationComments = List("on the other hand"))))
     */
    variationComments: List[Comment]
):
  export metas.*

  private[pgn] def toMove(context: Position): Option[(Position, Move)] =
    san(context).toOption.map: x =>
      val move = Move(
        san = x.toSanStr,
        comments = comments,
        glyphs = glyphs,
        variationComments = variationComments
      )
      (x.after, move)

type ParsedPgnTree = Node[PgnNodeData]

case class ParsedPgn(initialPosition: InitialComments, tags: Tags, tree: Option[ParsedPgnTree]):

  def mainline: List[San] =
    tree.fold(List.empty[San])(_.mainline.map(_.value.san))

  def mainlineWithMetas: List[SanWithMetas] =
    tree.fold(List.empty)(_.mainline.map(x => SanWithMetas(x.value.san, x.value.metas)))

  def toGame: Game =
    Game(tags)

  def toPosition: Position =
    Position(tags)

  def toPgn: Pgn =
    val positionWithMove = AndFullMoveNumber(tags.variant, tags.fen)
    Pgn(tags, initialPosition, treeToPgn(positionWithMove.position), positionWithMove.ply.next)

  private def treeToPgn(position: Position): Option[Node[Move]] =
    tree.flatMap:
      _.mapAccumlOption_(position): (ctx, d) =>
        d.toMove(ctx)
          .fold(ctx -> None)(_ -> _.some)

case class ParsedMainline[A](initialPosition: InitialComments, tags: Tags, moves: List[A]):

  def toGame: Game =
    Game(tags)

  def toPosition: Position =
    Position(tags)
