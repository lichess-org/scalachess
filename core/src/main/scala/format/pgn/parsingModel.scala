package chess
package format.pgn

import cats.syntax.all.*
import chess.Position.AndFullMoveNumber
import chess.format.Fen

// We don't support variation without move now,
// but we can in the future when we support null move
case class PgnNodeData(
    san: San,
    metas: Metas, // describes the position after the move `san` is played
    /** `variationComments` are comments before the first move of a variation. Example:
     * `1.d4 {the best move} ( { on the other hand } 1.e4 { is not as good } )`
     * => PgnNodeData(1.d4, Metas.empty, List(Node(1.e4, Metas(Comment("is not as good"), List("on the other hand")))))
     */
    variationComments: List[Comment]
):
  export metas.*

  private[pgn] def toMove(context: Position): Option[(Position, Move)] =
    san(context).toOption
      .map(x =>
        (
          x.after,
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

  def mainline: List[San] =
    tree.fold(List.empty[San])(_.mainline.map(_.value.san))

  def mainlineWithMetas: List[SanWithMetas] =
    tree.fold(List.empty)(_.mainline.map(x => SanWithMetas(x.value.san, x.value.metas)))

  // def play: (state: Position, moves: List[MoveOrDrop], error: Option[ErrorStr]) =
  //   Position(tags.variant, tags.fen).playMoves(mainline)

  def toPgn: Pgn =
    val positionWithMove = initContext(tags)
    Pgn(tags, initialPosition, treeToPgn(positionWithMove.position), positionWithMove.ply.next)

  private def initContext(tags: Tags): AndFullMoveNumber =
    val variant        = tags.variant | chess.variant.Standard
    inline def default = AndFullMoveNumber(Position.init(variant, White), FullMoveNumber.initial)
    tags.fen.flatMap(Fen.readWithMoveNumber(variant, _)) | default

  private def treeToPgn(context: Position): Option[Node[Move]] =
    tree.flatMap:
      _.mapAccumlOption_(context): (ctx, d) =>
        d.toMove(ctx)
          .fold(ctx -> None)(_ -> _.some)

case class ParsedMainline[A](initialPosition: InitialComments, tags: Tags, sans: List[A])

// Standard Algebraic Notation
sealed trait San extends Moveable:
  def apply(position: Position): Either[ErrorStr, MoveOrDrop]
  def rawString: Option[String] = None

case class Std(
    dest: Square,
    role: Role,
    capture: Boolean = false,
    file: Option[File] = None,
    rank: Option[Rank] = None,
    promotion: Option[PromotableRole] = None,
    override val rawString: Option[String] = None
) extends San:

  def apply(position: Position): Either[ErrorStr, chess.Move] =
    position
      .byPiece(position.color, role)
      .first: square =>
        if compare(file, square.file) && compare(rank, square.rank)
        then position.variant.move(position, square, dest, promotion).toOption
        else None
      .toRight(ErrorStr(s"Cannot play $this"))

  override def toString = s"$role ${dest.key}"

  private inline def compare[A](a: Option[A], b: A) = a.fold(true)(b ==)

case class Drop(role: Role, square: Square, override val rawString: Option[String] = None) extends San:

  def apply(position: Position): Either[ErrorStr, chess.Drop] =
    position.drop(role, square)

case class Castle(side: Side, override val rawString: Option[String] = None) extends San:

  def apply(position: Position): Either[ErrorStr, chess.Move] =

    import position.{ genCastling, ourKing, variant }
    if !variant.allowsCastling then ErrorStr(s"Cannot castle in $variant").asLeft
    else
      ourKing
        .flatMap: k =>
          genCastling(k)
            .filter(variant.kingSafety)
            .find(_.castle.exists(_.side == side))
        .toRight(ErrorStr(s"Cannot castle ${side.fold("kingside", "queenside")}"))

opaque type Sans = List[San]
object Sans extends TotalWrapper[Sans, List[San]]

case class Metas(check: Check, checkmate: Boolean, comments: List[Comment], glyphs: Glyphs)

case class SanWithMetas(san: San, metas: Metas):
  export metas.*
  export san.*

object Metas:
  val empty: Metas = Metas(Check.No, false, Nil, Glyphs.empty)
