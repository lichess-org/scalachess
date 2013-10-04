package chess
package format

case class UciMove(orig: Pos, dest: Pos, promotion: Option[PromotableRole]) {

  def uci = orig.key + dest.key + promotionString

  def piotr = orig.piotrStr + dest.piotrStr + promotionString

  def promotionString = promotion.fold("")(_.forsyth.toString)
}

object UciMove {

  def apply(move: String): Option[UciMove] = for {
    orig ← Pos.posAt(move take 2)
    dest ← Pos.posAt(move drop 2 take 2)
    promotion = move lift 5 flatMap Role.promotable
  } yield UciMove(orig, dest, promotion)

  def piotr(move: String): Option[UciMove] = for {
    orig ← move.headOption flatMap Pos.piotr
    dest ← move lift 1 flatMap Pos.piotr
    promotion = move lift 2 flatMap Role.promotable
  } yield UciMove(orig, dest, promotion)
}
