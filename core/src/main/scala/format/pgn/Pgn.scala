package chess
package format.pgn

import monocle.syntax.all.*

case class Pgn[A](tags: Tags, initial: InitialComments, tree: Option[Node[A]]):

  def render: SanEncoder[A] ?=> PgnStr = PgnStr:
    import SanEncoder.*
    val builder = new StringBuilder

    if tags.value.nonEmpty then builder.append(tags).addOne('\n').addOne('\n')
    if initial.comments.nonEmpty then builder.append(initial.comments.mkString("{ ", " } { ", " }\n"))
    tree.foreach(_.render(builder))
    tags(_.Result).foreach(x => builder.addOne(' ').append(x))

    builder.toString

  def updatePly(ply: Ply, f: A => A): SanEncoder[A] ?=> Option[Pgn[A]] =
    this.focus(_.tree.some).modifyA(_.modifyInMainline(_.ply == ply, _.updateValue(f)))

  def updateLastPly(f: A => A): Pgn[A] =
    this.focus(_.tree.some).modify(_.modifyLastMainlineNode(_.updateValue(f)))

  def modifyInMainline(ply: Ply, f: Node[A] => Node[A]): SanEncoder[A] ?=> Option[Pgn[A]] =
    this.focus(_.tree.some).modifyA(_.modifyInMainline(_.ply == ply, f))

  def moves: List[A] = tree.fold(Nil)(_.mainlineValues)

  def withEvent(title: String) =
    copy(tags = tags + Tag(_.Event, title))

private def glyphs(id: Int) =
  Glyph
    .find(id)
    .fold(Glyphs.empty): g =>
      Glyphs.fromList(List(g))

case class Move(
    ply: Ply,
    san: SanStr,
    comments: List[Comment] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    opening: Option[String] = None,
    result: Option[String] = None,
    // time left for the user who made the move, after he made it
    secondsLeft: Option[Int] = None,
    variationComments: List[Comment] = Nil
):

  private def clockString: Option[String] =
    secondsLeft.map(seconds => "[%clk " + Move.formatPgnSeconds(seconds) + "]")

  private def hasCommentsOrTime =
    comments.nonEmpty || secondsLeft.isDefined || opening.isDefined || result.isDefined

  def render(builder: StringBuilder) =
    builder.append(san.value)
    glyphs.toList.foreach:
      case glyph if glyph.id <= 6 => builder.append(glyph.symbol)
      case glyph                  => builder.append(" $").append(glyph.id)
    if hasCommentsOrTime then
      List(clockString, opening, result).flatten
        .:::(comments.map(_.map(Move.noDoubleLineBreak)))
        .foreach(x => builder.append(" { ").append(x).append(" }"))

object Move:

  given SanEncoder[Move] with
    extension (m: Move)
      def render(builder: StringBuilder) = m.render(builder)
      def isBlack                        = m.ply.turn.black
      def renderVariationComment(builder: StringBuilder) =
        m.variationComments.foreach(x => builder.append(" { ").append(x.value).append(" }"))
      def turnNumber = if m.ply.turn.black then m.ply.fullMoveNumber else m.ply.fullMoveNumber - 1
      def isLong     = m.comments.nonEmpty || m.secondsLeft.isDefined
      def ply        = m.ply

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  def formatPgnSeconds(t: Int): String =
    val d = java.time.Duration.ofSeconds(t)
    f"${d.toHours}:${d.toMinutesPart}%02d:${d.toSecondsPart}%02d"
