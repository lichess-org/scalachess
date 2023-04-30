package chess
package format.pgn

import cats.syntax.all.*

type PgnTree = Node[Move]

// isomorphic to Pgn
case class Pgn(tags: Tags, initial: Initial, tree: Option[PgnTree]):
  def render: PgnStr = PgnStr:
    import PgnTree.*
    val initStr =
      if initial.comments.nonEmpty then initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val turnStr: String = tree.fold("")(_.render)
    val resultStr       = tags(_.Result) | ""
    val endStr =
      if (turnStr.nonEmpty) s" $resultStr"
      else resultStr
    s"$tags\n\n$initStr$turnStr$endStr".trim

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
      val childStr = v.child.fold("")(x => s" ${x.render(d)}")
      s"$str$childStr"

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
    secondsLeft: Option[Int] = None
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

  def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  def formatPgnSeconds(t: Int): String =
    val d = java.time.Duration.ofSeconds(t)
    f"${d.toHours}:${d.toMinutesPart}%02d:${d.toSecondsPart}%02d"
