package chess
package format.pgn

import cats.syntax.all.*

// Standard Algebraic Notation
sealed trait San extends Moveable

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
