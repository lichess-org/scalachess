package chess
package format.pgn

import cats.data.Validated
import cats.syntax.option.*

opaque type Sans = List[San]
object Sans extends TotalWrapper[Sans, List[San]]

case class ParsedPgn(initialPosition: InitialPosition, tags: Tags, sans: Sans)

// Standard Algebraic Notation
sealed trait San:

  def apply(situation: Situation): Validated[ErrorStr, MoveOrDrop]

  def metas: Metas

  def withMetas(m: Metas): San

  def withSuffixes(s: Suffixes): San = withMetas(metas withSuffixes s)

  def withComments(s: List[Comment]): San = withMetas(metas withComments s)

  def withVariations(v: List[Variation]): San = withMetas(metas withVariations v)

  def mergeGlyphs(glyphs: Glyphs): San =
    withMetas(
      metas.withGlyphs(metas.glyphs merge glyphs)
    )

case class Std(
    dest: Square,
    role: Role,
    capture: Boolean = false,
    file: Option[File] = None,
    rank: Option[Rank] = None,
    promotion: Option[PromotableRole] = None,
    metas: Metas = Metas.empty
) extends San:

  def apply(situation: Situation) = move(situation)

  override def withSuffixes(s: Suffixes) =
    copy(
      metas = metas withSuffixes s,
      promotion = s.promotion
    )

  def withMetas(m: Metas) = copy(metas = m)

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

  def withMetas(m: Metas) = copy(metas = m)

  def drop(situation: Situation): Validated[ErrorStr, chess.Drop] =
    situation.drop(role, square)

opaque type Comment = String
object Comment extends TotalWrapper[Comment, String]

opaque type InitialPosition = List[Comment]
object InitialPosition extends TotalWrapper[InitialPosition, List[Comment]]:
  extension (ip: InitialPosition) inline def comments: List[Comment] = ip

// could be factored as `PgnNode` and used directly in `ParsedPgn` too
// not done for the moment for backward compat
// see `GameNode` in python-chess
case class Variation(comments: List[Comment], sans: Sans)

case class Metas(
    check: Boolean,
    checkmate: Boolean,
    comments: List[Comment],
    glyphs: Glyphs,
    variations: List[Variation]
):

  def withSuffixes(s: Suffixes) =
    copy(
      check = s.check,
      checkmate = s.checkmate,
      glyphs = s.glyphs
    )

  def withGlyphs(g: Glyphs) = copy(glyphs = g)

  def withComments(c: List[Comment]) = copy(comments = c)

  def withVariations(v: List[Variation]) = copy(variations = v)

object Metas:
  val empty = Metas(check = false, checkmate = false, Nil, Glyphs.empty, Nil)

case class Castle(
    side: Side,
    metas: Metas = Metas.empty
) extends San:

  def apply(situation: Situation) = move(situation)

  def withMetas(m: Metas) = copy(metas = m)

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
