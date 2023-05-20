package chess
package format.pgn

import cats.data.Validated
import cats.syntax.all.*
import chess.Node as CNode

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

type ParsedPgnTree = CNode[PgnNodeData]

case class ParsedPgn(initialPosition: InitialPosition, tags: Tags, tree: Option[ParsedPgnTree]):
  def mainline = tree.fold(List.empty[San])(_.mainline.map(_.value.san))

// Standard Algebraic Notation
sealed trait San:
  def apply(situation: Situation): Validated[ErrorStr, MoveOrDrop]

case class Std(
    dest: Square,
    role: Role,
    capture: Boolean = false,
    file: Option[File] = None,
    rank: Option[Rank] = None,
    promotion: Option[PromotableRole] = None
) extends San:

  def apply(situation: Situation) = move(situation)

  def move(situation: Situation): Validated[ErrorStr, chess.Move] =
    val pieces = situation.board.byPiece(situation.color - role)
    pieces.first { square =>
      if compare(file, square.file.index + 1) &&
        compare(rank, square.rank.index + 1)
      then situation.generateMovesAt(square) find { _.dest == dest }
      else None
    } match
      case None       => Validated invalid ErrorStr(s"No move found: $this\n$situation")
      case Some(move) => move withPromotion promotion toValid ErrorStr("Wrong promotion")

  override def toString = s"$role ${dest.key}"

  private inline def compare[A](a: Option[A], b: A) = a.fold(true)(b ==)

case class Drop(role: Role, square: Square) extends San:

  def apply(situation: Situation) = drop(situation)

  def drop(situation: Situation): Validated[ErrorStr, chess.Drop] =
    situation.drop(role, square)

case class Castle(side: Side) extends San:

  def apply(situation: Situation) = move(situation)

  def move(situation: Situation): Validated[ErrorStr, chess.Move] =
    import situation.{ genCastling, ourKing, variant }
    def error: ErrorStr = ErrorStr(s"Cannot castle / variant is $variant")
    if !variant.allowsCastling then error.invalid
    else
      ourKing.flatMap(k =>
        variant
          .applyVariantEffect(genCastling(k))
          .filter(variant.kingSafety)
          .find(_.castle.exists(_.side == side))
      ) toValid error

opaque type Sans = List[San]
object Sans extends TotalWrapper[Sans, List[San]]

opaque type InitialPosition = List[Comment]
object InitialPosition extends TotalWrapper[InitialPosition, List[Comment]]:
  extension (ip: InitialPosition) inline def comments: List[Comment] = ip

case class Metas(
    check: Check,
    checkmate: Boolean,
    comments: List[Comment],
    glyphs: Glyphs
)

object Metas:
  val empty = Metas(Check.No, checkmate = false, Nil, Glyphs.empty)
