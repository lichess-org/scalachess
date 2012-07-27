package chess
package format.pgn

case class Pgn(
    tags: List[Tag],
    turns: List[Turn]) {

  def updateTurn(fullMove: Int, f: Turn ⇒ Turn) = fullMove - 1 |> { index ⇒
    turns lift index fold (
      turn ⇒ copy(turns = turns.updated(index, f(turn))),
      this
    )
  }

  override def toString = "%s\n\n%s %s".format(
    tags mkString "\n",
    turns mkString " ",
    tags find (_.name == Tag.Result) map (_.value) filter ("*" !=) getOrElse ""
  ).trim
}

case class Turn(
    number: Int,
    white: Option[Move],
    black: Option[Move]) {

  def update(color: Color, f: Move ⇒ Move) = color.fold(
    copy(white = white map f),
    copy(black = black map f)
  )

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
    variation: List[Turn] = Nil,
    timeLeft: Option[Int] = None) {

  def isLong = comment.isDefined || variation.nonEmpty

  def timeLeftString: Option[String] = timeLeft.fold(
    tlValue => Some(List(List(0, 3600),List(0, 60),List(0, 1)).scanLeft(
      List(tlValue, 0))({case (List(x,a),List(b,y)) => List(x%y, x/y)}).
        tail.transpose.tail.flatten.map("%02d".format(_)).mkString(":")), None)

  override def toString = "%s%s%s".format(
    san,
    nag.fold(" $" + _, ""),
    (comment.isDefined || timeLeft.isDefined).fold(
      List(
        " {",
        timeLeftString.getOrElse(""),
        comment.getOrElse(""), "}"
      ).flatten.mkString(" "),
      ""
    )
  )
}
