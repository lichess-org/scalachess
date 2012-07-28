package chess
package format.pgn

import org.joda.time.Duration
import org.joda.time.format.PeriodFormatterBuilder

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
    // time left for the user who made the move, after he made it
    timeLeft: Option[Int] = None) {

  def isLong = comment.isDefined || variation.nonEmpty

  def timeLeftString: Option[String] =
    timeLeft.map(time ⇒ "[%clk " + Move.formatTime(time) + "]")

  override def toString = "%s%s%s".format(
    san,
    nag.fold(" $" + _, ""),
    (comment.isDefined || timeLeft.isDefined).fold(
      List(timeLeftString, comment).flatten.mkString(" { ", " ", " }"),
      ""
    )
  )
}
object Move {

  def formatTime(time: Int) = periodFormatter.print(
    Duration.standardSeconds(time).toPeriod
  )

  private val periodFormatter = new PeriodFormatterBuilder().
    printZeroAlways.
    minimumPrintedDigits(1).appendHours.appendSeparator(":").
    minimumPrintedDigits(2).appendMinutes.appendSeparator(":").
    appendSeconds.
    toFormatter
}

