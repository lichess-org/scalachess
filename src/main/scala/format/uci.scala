package chess
package format

case class UciMove(orig: Pos, dest: Pos, promotion: Option[PromotableRole]) {

  def uci = orig.key + dest.key + promotionString

  def piotr = orig.piotr + dest.piotr + promotionString

  def promotionString = promotion.fold(_.forsyth.toString, "")
}

object UciMove {

  def apply(move: String): Option[UciMove] = for {
    orig ← Pos.posAt(move take 2)
    dest ← Pos.posAt(move drop 2 take 2)
    promotion = (move drop 4).headOption flatMap Role.promotable
  } yield UciMove(orig, dest, promotion)

  def piotr(move: String): Option[UciMove] = for {
    orig ← move.headOption flatMap Pos.piotr
    dest ← move.tail.headOption flatMap Pos.piotr
    promotion = move.tail.tail.headOption flatMap Role.promotable
  } yield UciMove(orig, dest, promotion)
}
