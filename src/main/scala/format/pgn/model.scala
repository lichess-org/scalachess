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
    white.fold(
      w ⇒ black.fold(
        b ⇒ "%s %s".format(w, b),
        w),
      ~(black map (_.toString))
    )
  )
}

case class Move(
    san: String,
    nag: Option[Int] = None,
    comment: Option[String] = None,
    variation: Option[Movetext] = None) {

  override def toString = "%s%s%s".format(
    san,
    nag.fold(" $" + _, ""),
    comment.fold(" { " + _ + " }", "")
  )
}
