package chess
package format.pgn

import cats.syntax.all.*

type PgnTree = Node[NewMove]

// isomorphic to Pgn
case class NewPgn(tags: Tags, initial: Initial, tree: Option[PgnTree]):
  def toPgn: Pgn =
    val moves = tree.fold(List.empty[Move])(toMove(_, Ply(1)))
    val turns = Turn.fromMoves(moves, Ply(1))
    Pgn(tags, turns, initial)

  def toMove(node: PgnTree, ply: Ply): List[Move] =
    val variations = node.variations.map(x => Turn.fromMoves(toMove(x, ply), ply))
    val move       = toMove(node.value, variations)
    move :: node.child.fold(Nil)(toMove(_, ply + 1))

  def toMove(move: NewMove, variations: List[List[Turn]]): Move =
    Move(
      san = move.san,
      comments = move.comments,
      glyphs = move.glyphs,
      opening = move.opening,
      result = move.result,
      variations = variations,
      secondsLeft = move.secondsLeft
    )

  def render: PgnStr = PgnStr:
    val initStr =
      if initial.comments.nonEmpty then initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val turnStr: String = ???
    val resultStr       = tags(_.Result) | ""
    val endStr =
      if (turnStr.nonEmpty) s" $resultStr"
      else resultStr
    s"$tags\n\n$initStr$turnStr$endStr".trim

object PgnTree:
  extension (tree: PgnTree)
    def isLong = tree.value.isLong || tree.variation.isDefined
    def _render: String =
      val moveStr = tree.value.toString
      val varStr =
        if tree.variation.isEmpty then ""
        else tree.variations.map(x => s" ($x )").mkString(" ")
      s"$moveStr$varStr"

    def render: (Boolean, String) =
      render(!tree.value.ply.color.white)

    def render(dot: Boolean): (Boolean, String) =
      if tree.value.ply.color.white then (isLong, s" ${tree.value.ply.fullMoveNumber}.$_render")
      else
        val number = if dot then s"${tree.value.ply.fullMoveNumber}..." else ""
        (false, s" $number$_render")

object NewPgn:
  def moves(turn: Turn): List[(Ply, Move)] =
    List(
      turn.white.map(m => turn.plyOf(White) -> m),
      turn.black.map(m => turn.plyOf(Black) -> m)
    ).flatten
  def moves(pgn: Pgn): List[(Ply, Move)] = pgn.turns.flatMap(moves)

  extension (move: Move)
    def clean(ply: Ply): NewMove = NewMove(
      ply = ply,
      san = move.san,
      comments = move.comments,
      glyphs = move.glyphs,
      opening = move.opening,
      result = move.result,
      secondsLeft = move.secondsLeft
    )

  def apply(pgn: Pgn): NewPgn =
    val tree = moves(pgn).reverse.foldLeft(none[PgnTree]) { (o, move) => Some(toNode(move._2, move._1, o)) }
    NewPgn(tags = pgn.tags, initial = pgn.initial, tree = tree)

  def toNode(move: Move, ply: Ply, child: Option[PgnTree]): PgnTree =
    Node(
      move.clean(ply),
      child,
      move.variations.map(_.flatMap(moves)).map(x => toNode(x.head._2, ply, None)).toVariations
    )

private def glyphs(id: Int) =
  Glyph.find(id).fold(Glyphs.empty) { g =>
    Glyphs fromList List(g)
  }

case class NewMove(
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
