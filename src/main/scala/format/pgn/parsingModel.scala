package chess
package format.pgn

import cats.data.Validated
import cats.syntax.option.*

case class PgnNodeData(san: San, metas: Metas, variationComments: Option[List[Comment]])
type ParsedPgnTree = Node[PgnNodeData]

case class ParsedPgn(initialPosition: InitialPosition, tags: Tags, tree: Option[ParsedPgnTree]):
  def mainLine = tree.fold(List.empty[San])(_.mainLine.map(_.san))

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

case class Drop(
    role: Role,
    square: Square,
    metas: Metas = Metas.empty
) extends San:

  def apply(situation: Situation) = drop(situation)

  def drop(situation: Situation): Validated[ErrorStr, chess.Drop] =
    situation.drop(role, square)

case class Castle(side: Side) extends San:

  def apply(situation: Situation) = move(situation)

  def move(situation: Situation): Validated[ErrorStr, chess.Move] =
    import situation.{ genCastling, ourKing, variant }
    ourKing.flatMap(k =>
      variant
        .applyVariantEffect(genCastling(k))
        .filter(variant.kingSafety)
        .find(_.castle.exists(_.side == side))
    ) toValid ErrorStr(s"Cannot castle / variant is $variant")

opaque type Sans = List[San]
object Sans extends TotalWrapper[Sans, List[San]]

opaque type Comment = String
object Comment extends TotalWrapper[Comment, String]:
  extension (cs: List[Comment])
    inline def cleanUp: List[Comment] =
      cs.collect { case c if !c.isBlank => c.trim }

opaque type InitialPosition = List[Comment]
object InitialPosition extends TotalWrapper[InitialPosition, List[Comment]]:
  extension (ip: InitialPosition) inline def comments: List[Comment] = ip

case class Metas(
    check: Boolean,
    checkmate: Boolean,
    comments: List[Comment],
    glyphs: Glyphs
)

object Metas:
  val empty = Metas(check = false, checkmate = false, Nil, Glyphs.empty)
