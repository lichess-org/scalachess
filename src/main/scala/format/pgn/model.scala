package chess
package format.pgn

case class Pgn(
    tags: List[Tag],
    movetext: Movetext) {

  override def toString = "%s\n\n%s".format(tags mkString "\n", movetext)
}

case class Movetext(
    turns: List[Turn]) {

  override def toString = turns mkString " "
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
    variation: Option[Movetext] = None) {

  def isLong = comment.isDefined || variation.isDefined

  override def toString = "%s%s%s".format(
    san,
    nag.fold(" $" + _, ""),
    comment.fold(" { " + _ + " }", "")
  )
}
