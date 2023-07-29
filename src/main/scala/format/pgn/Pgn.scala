package chess
package format.pgn

import monocle.syntax.all.*

type PgnTree = Node[Move]

case class Pgn(tags: Tags, initial: InitialComments, tree: Option[PgnTree]):

  def render: PgnStr = PgnStr:
    import PgnTree.*
    val builder = new StringBuilder

    if tags.value.nonEmpty then builder.append(tags).addOne('\n').addOne('\n')
    if initial.comments.nonEmpty then builder.append(initial.comments.mkString("{ ", " } { ", " }\n"))
    tree.foreach(_.render(builder))
    tags(_.Result).foreach(x => builder.addOne(' ').append(x))

    builder.toString

  def updatePly(ply: Ply, f: Move => Move): Option[Pgn] =
    this.focus(_.tree.some).modifyA(_.modifyInMainline(_.ply == ply, _.updateValue(f)))

  def updateLastPly(f: Move => Move): Pgn =
    this.focus(_.tree.some).modify(_.modifyLastMainlineNode(_.updateValue(f)))

  def modifyInMainline(ply: Ply, f: Node[Move] => Node[Move]): Option[Pgn] =
    this.focus(_.tree.some).modifyA(_.modifyInMainline(_.ply == ply, f))

  def moves: List[Move] = tree.fold(Nil)(_.mainlineValues)

  def withEvent(title: String) =
    copy(tags = tags + Tag(_.Event, title))

object PgnTree:

  extension (tree: Tree[Move])
    def isLong = tree.value.isLong || tree.variations.nonEmpty

    def render(builder: StringBuilder): Unit =
      render(builder, !tree.value.ply.turn.black)

    @annotation.tailrec
    def render(builder: StringBuilder, dot: Boolean): Unit =
      if tree.isVariation then builder.append(Move.render(tree.value.variationComments))
      val d = tree.prefix(dot, builder)
      renderValueAndVariations(builder)
      tree.child match
        case None => ()
        case Some(x) =>
          builder.addOne(' ')
          x.render(builder, d)

    def prefix(dot: Boolean, builder: StringBuilder): Boolean =
      if tree.value.ply.turn.black then
        builder.append(tree.value.turnNumber).append(". ")
        tree.isLong
      else
        if dot then builder.append(tree.value.turnNumber).append("... ")
        false

    def renderValueAndVariations(builder: StringBuilder) =
      tree.value.render(builder)
      tree.variations.foreach: x =>
        builder.addOne(' ').addOne('(')
        x.render(builder)
        builder.addOne(')')

private def glyphs(id: Int) =
  Glyph
    .find(id)
    .fold(Glyphs.empty): g =>
      Glyphs fromList List(g)

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
  def turnNumber = if ply.turn.black then ply.fullMoveNumber else ply.fullMoveNumber - 1
  def isLong     = comments.nonEmpty || secondsLeft.isDefined

  private def clockString: Option[String] =
    secondsLeft.map(seconds => "[%clk " + Move.formatPgnSeconds(seconds) + "]")

  private def hasCommentsOrTime =
    comments.nonEmpty || secondsLeft.isDefined || opening.isDefined || result.isDefined

  def render(builder: StringBuilder) =
    builder.append(san)
    glyphs.toList.foreach:
      case glyph if glyph.id <= 6 => builder.append(glyph.symbol)
      case glyph                  => builder.append(" $").append(glyph.id)

    if hasCommentsOrTime then
      List(clockString, opening, result).flatten
        .:::(comments.map(_ map Move.noDoubleLineBreak))
        .foreach(x => builder.append(" { ").append(x).append(" }"))

  def render =
    val glyphStr = glyphs.toList
      .map:
        case glyph if glyph.id <= 6 => glyph.symbol
        case glyph                  => s" $$${glyph.id}"
      .mkString
    val commentsOrTime =
      if hasCommentsOrTime then
        List(clockString, opening, result).flatten
          .:::(comments.map(_ map Move.noDoubleLineBreak))
          .map(x => s" { $x }")
          .mkString
      else ""
    s"$san$glyphStr$commentsOrTime"

object Move:

  val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  def render(cm: List[Comment]): String =
    val builder = new StringBuilder
    cm.foreach(x => builder.append(" { ").append(x.value).append(" }"))
    builder.toString

  def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  def formatPgnSeconds(t: Int): String =
    val d = java.time.Duration.ofSeconds(t)
    f"${d.toHours}:${d.toMinutesPart}%02d:${d.toSecondsPart}%02d"
