package chess
package format.pgn

import monocle.syntax.all.*

type PgnTree = Node[Move]

case class Pgn(tags: Tags, initial: Initial, tree: Option[PgnTree]):

  def render: PgnStr = PgnStr:
    toString

  override def toString(): String =
    import PgnTree.*
    val initStr =
      if initial.comments.nonEmpty then initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val movesStr  = tree.fold("")(_.render)
    val resultStr = tags(_.Result) | ""
    val endStr =
      if movesStr.nonEmpty then s" $resultStr"
      else resultStr
    s"$tags\n\n$initStr$movesStr$endStr".trim

  def updatePly(ply: Ply, f: Move => Move): Option[Pgn] =
    val predicate = (m: Move) => m.ply == ply
    this.focus(_.tree.some).modifyA(_.modifyInMainline(predicate, Node.lift(f)))

  def updateLastPly(f: Move => Move): Pgn =
    this.focus(_.tree.some).modify(_.modifyLastMainlineNode(Node.lift(f)))

  def moves: List[Move] = tree.fold(List.empty[Move])(_.mainlineValues)

  def withEvent(title: String) =
    copy(tags = tags + Tag(_.Event, title))

object PgnTree:
  extension (tree: PgnTree)
    def isLong = tree.value.isLong || tree.variations.nonEmpty
    def _render: String =
      val moveStr = tree.value.toString
      val varStr =
        if tree.variations.isEmpty then ""
        else tree.variations.map(x => s" (${x.render})").mkString(" ")
      s"$moveStr$varStr"

    def render: String =
      render(!tree.value.ply.color.white)

    def render(dot: Boolean): String =
      val (d, str) =
        if tree.value.ply.color.white then (isLong, s"${tree.value.ply.fullMoveNumber}. $_render")
        else
          val number = if dot then s"${tree.value.ply.fullMoveNumber}... " else ""
          (false, s"$number$_render")
      val childStr = tree.child.fold("")(x => s" ${x.render(d)}")
      s"$str$childStr"

  extension (v: Variation[Move])
    def _render: String =
      v.value.toString

    def render: String =
      render(!v.value.ply.color.white)

    def render(dot: Boolean): String =
      val (d, str) =
        if v.value.ply.color.white then (v.value.isLong, s"${v.value.ply.fullMoveNumber}. $_render")
        else
          val number = if dot then s"${v.value.ply.fullMoveNumber}... " else ""
          (false, s"$number$_render")
      val childStr          = v.child.fold("")(x => s" ${x.render(d)}")
      val variationComments = Move.render(v.value.variationComments)
      s"$variationComments$str$childStr"

private def glyphs(id: Int) =
  Glyph.find(id).fold(Glyphs.empty) { g =>
    Glyphs fromList List(g)
  }

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

  def isLong = comments.nonEmpty || secondsLeft.isDefined

  private def clockString: Option[String] =
    secondsLeft.map(seconds => "[%clk " + Move.formatPgnSeconds(seconds) + "]")

  override def toString =
    val glyphStr = glyphs.toList.map {
      case glyph if glyph.id <= 6 => glyph.symbol
      case glyph                  => s" $$${glyph.id}"
    }.mkString
    val commentsOrTime =
      if (comments.nonEmpty || secondsLeft.isDefined || opening.isDefined || result.isDefined)
        List(clockString, opening, result).flatten
          .:::(comments.map(_ map Move.noDoubleLineBreak))
          .map { text =>
            s" { $text }"
          }
          .mkString
      else ""
    s"$san$glyphStr$commentsOrTime"

object Move:

  val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  def render(cm: List[Comment]): String =
    cm.foldLeft("")((acc, x) => acc ++ s" { ${noDoubleLineBreak(x.value)} }")

  def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  def formatPgnSeconds(t: Int): String =
    val d = java.time.Duration.ofSeconds(t)
    f"${d.toHours}:${d.toMinutesPart}%02d:${d.toSecondsPart}%02d"
