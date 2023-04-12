package chess
package format.pgn

import cats.data.Validated
import cats.syntax.option.*

opaque type Sans = List[San]
object Sans extends TotalWrapper[Sans, List[San]]

// Standard Algebraic Notation
sealed trait San:

  def apply(situation: Situation): Validated[ErrorStr, MoveOrDrop]

case class Std(
    dest: Pos,
    role: Role,
    capture: Boolean = false,
    file: Option[File] = None,
    rank: Option[Rank] = None,
    promotion: Option[PromotableRole] = None,
) extends San:

  def apply(situation: Situation) = move(situation)

  def move(situation: Situation): Validated[ErrorStr, chess.Move] =
    val pieces = situation.board.byPiece(situation.color - role)
    pieces.first { pos =>
      if compare(file, pos.file.index + 1) &&
        compare(rank, pos.rank.index + 1)
      then situation.generateMovesAt(pos) find { _.dest == dest }
      else None
    } match
      case None       => Validated invalid ErrorStr(s"No move found: $this\n$situation")
      case Some(move) => move withPromotion promotion toValid ErrorStr("Wrong promotion")

  // override def toString = s"$role ${dest.key}"

  private inline def compare[A](a: Option[A], b: A) = a.fold(true)(b ==)

case class Drop(
    role: Role,
    pos: Pos,
) extends San:

  def apply(situation: Situation) = drop(situation)

  def drop(situation: Situation): Validated[ErrorStr, chess.Drop] =
    situation.drop(role, pos)

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
    glyphs: Glyphs,
):

  def withSuffixes(s: Suffixes) =
    copy(
      check = s.check,
      checkmate = s.checkmate,
      glyphs = s.glyphs
    )

  def withGlyphs(g: Glyphs) = copy(glyphs = g)

  def withComments(c: List[Comment]) = copy(comments = c)

object Metas:
  val empty = Metas(check = false, checkmate = false, Nil, Glyphs.empty)

case class Castle( side: Side) extends San:

  def apply(situation: Situation) = move(situation)

  def move(situation: Situation): Validated[ErrorStr, chess.Move] =
    import situation.{ genCastling, ourKing, variant }
    ourKing.flatMap(k =>
      variant
        .applyVariantEffect(genCastling(k))
        .filter(variant.kingSafety)
        .find(_.castle.exists(_.side == side))
    ) toValid ErrorStr(s"Cannot castle / variant is $variant")

case class Suffixes(
    check: Boolean,
    checkmate: Boolean,
    promotion: Option[PromotableRole],
    glyphs: Glyphs
)
