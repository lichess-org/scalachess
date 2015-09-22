package chess
package format

case class UciMove(orig: Pos, dest: Pos, promotion: Option[PromotableRole] = None) {

  def keys = orig.key + dest.key
  def uci = keys + promotionString

  def keysPiotr = orig.piotrStr + dest.piotrStr
  def piotr = keysPiotr + promotionString

  def promotionString = promotion.fold("")(_.forsyth.toString)

  def origDest = orig -> dest
}

object UciMove
    extends scalaz.std.OptionInstances
    with scalaz.syntax.ToTraverseOps {

  def apply(move: Move): UciMove = UciMove(move.orig, move.dest, move.promotion)

  def apply(move: String): Option[UciMove] = for {
    orig ← Pos.posAt(move take 2)
    dest ← Pos.posAt(move drop 2 take 2)
    promotion = move lift 4 flatMap Role.promotable
  } yield UciMove(orig, dest, promotion)

  def piotr(move: String): Option[UciMove] = for {
    orig ← move.headOption flatMap Pos.piotr
    dest ← move lift 1 flatMap Pos.piotr
    promotion = move lift 2 flatMap Role.promotable
  } yield UciMove(orig, dest, promotion)

  def readList(moves: String): Option[List[UciMove]] =
    moves.split(' ').toList.map(apply).sequence

  def writeList(moves: List[UciMove]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[UciMove]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[UciMove]): String =
    moves.map(_.piotr) mkString " "
}
