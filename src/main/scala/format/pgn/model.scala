package chess
package format
package pgn

import scala._

case class Pgn(
    tags: List[Tag],
    turns: List[Turn]) {

  def updateTurn(fullMove: Int, f: Turn => Turn) = {
    val index = fullMove - 1
    (turns lift index).fold(this) { turn =>
      copy(turns = turns.updated(index, f(turn)))
    }
  }
  def updatePly(ply: Int, f: Move => Move) = {
    val fullMove = (ply + 1) / 2
    val color = Color(ply % 2 == 1)
    updateTurn(fullMove, _.update(color, f))
  }
  def updateLastPly(f: Move => Move) = updatePly(nbPlies, f)

  def nbPlies = turns.foldLeft(0)(_ + _.count)

  def moves = turns.flatMap { t =>
    List(t.white, t.black).flatten
  }

  def withEvent(title: String) = copy(
    tags = Tag(_.Event, title) :: tags.filterNot(_.name == Tag.Event)
  )

  override def toString = "%s\n\n%s %s".format(
    tags mkString "\n",
    turns mkString " ",
    tags find (_.name == Tag.Result) map (_.value) getOrElse ""
  ).trim
}

case class Turn(
    number: Int,
    white: Option[Move],
    black: Option[Move]) {

  def update(color: Color, f: Move => Move) = color.fold(
    copy(white = white map f),
    copy(black = black map f)
  )

  def updateLast(f: Move => Move) = {
    black.map(m => copy(black = f(m).some)) orElse
      white.map(m => copy(white = f(m).some))
  } | this

  def isEmpty = white.isEmpty && black.isEmpty

  def plyOf(color: Color) = number * 2 - color.fold(1, 0)

  def count = List(white, black) count (_.isDefined)

  override def toString = {
    val text = (white, black) match {
      case (Some(w), Some(b)) if w.isLong => s" $w $number... $b"
      case (Some(w), Some(b))             => s" $w $b"
      case (Some(w), None)                => s" $w"
      case (None, Some(b))                => s".. $b"
      case _                              => ""
    }
    s"$number.$text"
  }
}

object Turn {

  def fromMoves(moves: List[Move], ply: Int): List[Turn] = {
    moves.foldLeft((List[Turn](), ply)) {
      case ((turns, p), move) if p % 2 == 1 =>
        (Turn((p + 1) / 2, move.some, none) :: turns) -> (p + 1)
      case ((Nil, p), move) =>
        (Turn((p + 1) / 2, none, move.some) :: Nil) -> (p + 1)
      case ((t :: tt, p), move) =>
        (t.copy(black = move.some) :: tt) -> (p + 1)
    }
  }._1.reverse
}

case class Move(
    san: String,
    nag: Option[Int] = None,
    comment: Option[String] = None,
    opening: Option[String] = None,
    result: Option[String] = None,
    variation: List[Turn] = Nil,
    // time left for the user who made the move, after he made it
    timeLeft: Option[Int] = None) {

  def isLong = comment.isDefined || variation.nonEmpty

  def timeString(time: Int) = Clock.timeString(time)

  private def clockString: Option[String] =
    timeLeft.map(time => "[%clk " + timeString(time) + "]")

  override def toString = {
    val nagSymbol = nag.fold("") { code => Nag(code).fold(" $" + code)(_.symbol) }
    val commentOrTime =
      if (comment.isDefined || timeLeft.isDefined || opening.isDefined || result.isDefined)
        List(clockString, opening, result, comment).flatten.mkString(" { ", " ", " }")
      else ""
    val variationString = if (variation.isEmpty) "" else variation.mkString(" (", " ", ")")
    s"$san$nagSymbol$commentOrTime$variationString"
  }
}
