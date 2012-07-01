package chess
package format.pgn

case class Pgn(
    tags: List[Tag],
    turns: List[Turn]) {

  override def toString = "%s\n\n%s".format(
    tags mkString "\n", 
    turns mkString " ")
}

case class Turn(
    number: Int,
    white: Option[Move],
    black: Option[Move]) {

  override def toString = "%d.%s".format(
    number,
    (white, black) match {
      case (Some(w), Some(b)) if w.isLong ⇒ " %s %d... %s".format(w, number, b)
      case (Some(w), Some(b))             ⇒ " %s %s".format(w, b)
      case (Some(w), None)                ⇒ " %s".format(w)
      case (None, Some(b))                ⇒ ".. %s".format(b)
      case (None, None)                   ⇒ ""
    }
  )
}

case class Move(
    san: String,
    nag: Option[Int] = None,
    comment: Option[String] = None,
    variation: List[Turn] = Nil) {

  def isLong = comment.isDefined || variation.nonEmpty

  override def toString = "%s%s%s".format(
    san,
    nag.fold(" $" + _, ""),
    comment.fold(" { " + _ + " }", "")
  )
}
