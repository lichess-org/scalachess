package chess
package format.pgn

import cats.syntax.all.*
import chess.format.Fen

import MoveOrDrop.*

// We don't support variation without move now,
// but we can in the future when we support null move
case class PgnNodeData(
    san: San,
    metas: Metas, // describes the position after the move `san` is played
    /* `variationComments` are comments before the first move of a variation. Example:
     * `1.d4 {the best move} ( { on the other hand } 1.e4 { is not as good } )`
     * => PgnNodeData(1.d4, Metas.empty, List(Node(1.e4, Metas(Comment("is not as good"), List("on the other hand")))
     */
    variationComments: List[Comment]
):
  export metas.*

  private[pgn] def toMove(context: Situation): Option[(Situation, Move)] =
    san(context).toOption
      .map(x =>
        (
          x.situationAfter,
          Move(
            san = x.toSanStr,
            comments = comments,
            glyphs = glyphs,
            variationComments = variationComments
          )
        )
      )

type ParsedPgnTree = Node[PgnNodeData]

case class ParsedPgn(initialPosition: InitialComments, tags: Tags, tree: Option[ParsedPgnTree]):
  def mainline: List[San] = tree.fold(List.empty[San])(_.mainline.map(_.value.san))

  def toPgn: Pgn =
    val sitWithMove = initContext(tags)
    Pgn(tags, initialPosition, treeToPgn(sitWithMove.situation), sitWithMove.ply.next)

  private def initContext(tags: Tags) =
    val variant = tags.variant | chess.variant.Standard
    def default = Situation.AndFullMoveNumber(Situation(Board.init(variant), White), FullMoveNumber.initial)

    tags.fen
      .flatMap(Fen.readWithMoveNumber(variant, _))
      .getOrElse(default)

  private def treeToPgn(context: Situation): Option[Node[Move]] =
    tree.flatMap:
      _.mapAccumlOption_(context): (ctx, d) =>
        d.toMove(ctx)
          .fold(ctx -> None)(_ -> _.some)

// Standard Algebraic Notation
sealed trait San:
  def apply(situation: Situation): Either[ErrorStr, MoveOrDrop]

case class Std(
    dest: Square,
    role: Role,
    capture: Boolean = false,
    file: Option[File] = None,
    rank: Option[Rank] = None,
    promotion: Option[PromotableRole] = None
) extends San:

  def apply(situation: Situation) = move(situation)

  def move(situation: Situation): Either[ErrorStr, chess.Move] =
    situation.board
      .byPiece(situation.color - role)
      .first: square =>
        if compare(file, square.file) && compare(rank, square.rank)
        then situation.generateMovesAt(square).find(_.dest == dest)
        else None
      .toRight(ErrorStr(s"No move found: $this\n$situation"))
      .flatMap(_.withPromotion(promotion).toRight(ErrorStr("Wrong promotion")))

  override def toString = s"$role ${dest.key}"

  private inline def compare[A](a: Option[A], b: A) = a.fold(true)(b ==)

case class Drop(role: Role, square: Square) extends San:

  def apply(situation: Situation) = drop(situation)

  def drop(situation: Situation): Either[ErrorStr, chess.Drop] =
    situation.drop(role, square)

case class Castle(side: Side) extends San:

  def apply(situation: Situation) = move(situation)

  def move(situation: Situation): Either[ErrorStr, chess.Move] =
    import situation.{ genCastling, ourKing, variant }
    def error: ErrorStr = ErrorStr(s"Cannot castle / variant is $variant")
    if !variant.allowsCastling then error.asLeft
    else
      ourKing
        .flatMap: k =>
          variant
            .applyVariantEffect(genCastling(k))
            .filter(variant.kingSafety)
            .find(_.castle.exists(_.side == side))
        .toRight(error)

opaque type Sans = List[San]
object Sans extends TotalWrapper[Sans, List[San]]

case class Metas(check: Check, checkmate: Boolean, comments: List[Comment], glyphs: Glyphs)

object Metas:
  val empty = Metas(Check.No, checkmate = false, Nil, Glyphs.empty)
