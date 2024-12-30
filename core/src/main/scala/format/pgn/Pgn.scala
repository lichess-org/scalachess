package chess
package format.pgn

import cats.syntax.all.*
import monocle.syntax.all.*

type PgnTree = Node[Move]

object PgnTree:
  extension (tree: PgnTree)
    def render(ply: Ply) =
      import PgnNodeEncoder.*
      val builder = new StringBuilder
      tree.appendPgnStr(builder, ply)
      builder.toString

case class Pgn(tags: Tags, initial: InitialComments, tree: Option[PgnTree], startPly: Ply):

  def render: PgnStr = PgnStr:
    toString

  override def toString: String =
    import PgnNodeEncoder.*
    val builder = new StringBuilder

    if tags.value.nonEmpty then builder.append(tags).addOne('\n').addOne('\n')
    if initial.comments.nonEmpty then builder.append(initial.comments.mkString("{ ", " } { ", " }\n"))
    tree.foreach(_.appendPgnStr(builder, startPly))
    tags(_.Result).foreach(x => builder.addOne(' ').append(x))

    builder.toString

  def updatePly(ply: Ply, f: Move => Move): Option[Pgn] =
    this.focus(_.tree.some).modifyA(_.modifyInMainlineAt((ply - startPly).value, _.updateValue(f)))

  def updateLastPly(f: Move => Move): Pgn =
    this.focus(_.tree.some).modify(_.modifyLastMainlineNode(_.updateValue(f)))

  def modifyInMainline(ply: Ply, f: PgnTree => PgnTree): Option[Pgn] =
    this.focus(_.tree.some).modifyA(_.modifyInMainlineAt((ply - startPly).value, f))

  def moves: List[Move] = tree.fold(Nil)(_.mainlineValues)

  def withEvent(title: String) =
    copy(tags = tags + Tag(_.Event, title))

private def glyphs(id: Int) =
  Glyph
    .find(id)
    .fold(Glyphs.empty): g =>
      Glyphs.fromList(List(g))

case class Move(
    san: SanStr,
    comments: List[Comment] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    opening: Option[String] = None,
    result: Option[String] = None,
    secondsLeft: Option[Int] = None, // %clk clock in seconds for the move player, after the move
    moveTime: Option[Int] = None,    // %emt estimated move time in seconds
    variationComments: List[Comment] = Nil
):

  private def clockString: Option[String] = List(
    secondsLeft.map(seconds => "[%clk " + Move.formatPgnSeconds(seconds) + "]"),
    moveTime.map(seconds => "[%emt " + Move.formatPgnSeconds(seconds) + "]")
  ).flatten.some.filter(_.nonEmpty).map(_.mkString(" "))

  def hasComment = comments.nonEmpty || secondsLeft.isDefined || moveTime.isDefined

  private def nonEmpty = hasComment || opening.isDefined || result.isDefined

  private def appendSanStr(builder: StringBuilder): Unit =
    builder.append(san.value)
    glyphs.toList.foreach:
      case glyph if glyph.id <= 6 => builder.append(glyph.symbol)
      case glyph                  => builder.append(" $").append(glyph.id)
    if nonEmpty then
      List(clockString, opening, result).flatten
        .:::(comments.map(_.map(Move.noDoubleLineBreak)))
        .foreach(x => builder.append(" { ").append(x).append(" }"))

  def render: String =
    val builder = new StringBuilder
    appendSanStr(builder)
    builder.toString

object Move:

  given PgnNodeEncoder[Move] with
    extension (m: Move)
      def appendSanStr(builder: StringBuilder) = m.appendSanStr(builder)
      def appendVariationComment(builder: StringBuilder) =
        m.variationComments.foreach(x => builder.append(" { ").append(x.value).append(" }"))
      def hasComment = m.hasComment

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  def formatPgnSeconds(t: Int): String =
    val d = java.time.Duration.ofSeconds(t)
    f"${d.toHours}:${d.toMinutesPart}%02d:${d.toSecondsPart}%02d"
