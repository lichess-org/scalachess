package chess
package format.pgn

case class Glyph(id: Int, symbol: String, name: String) {

  override def toString = s"$symbol ($$$id $name)"
}

case class Glyphs(
    move: Option[Glyph.MoveAssessment],
    position: Option[Glyph.PositionAssessment],
    observations: List[Glyph.Observation]) {

  def isEmpty = move.isEmpty && position.isEmpty && observations.isEmpty

  def nonEmpty: Option[Glyphs] = if (isEmpty) None else Some(this)
}

object Glyphs {
  val empty = Glyphs(None, None, Nil)
}

object Glyph {

  sealed trait MoveAssessment extends Glyph

  object MoveAssessment {
    val good = new Glyph(1, "!", "Good move") with MoveAssessment
    val poor = new Glyph(2, "?", "Mistake") with MoveAssessment
    val brillant = new Glyph(3, "!!", "Brillant move") with MoveAssessment
    val blunder = new Glyph(4, "??", "Blunder") with MoveAssessment
    val speculative = new Glyph(5, "!?", "Speculative move") with MoveAssessment
    val questionable = new Glyph(6, "?!", "Questionable move") with MoveAssessment
    val only = new Glyph(7, "□", "Only move") with MoveAssessment

    val all = List(good, poor, brillant, blunder, speculative, questionable, only)
    val byId = all.map { g => g.id -> g }.toMap
  }

  sealed trait PositionAssessment extends Glyph

  object PositionAssessment {
    val equal = new Glyph(10, "=", "Even position") with PositionAssessment
    val unclear = new Glyph(13, "∞", "Unclear position") with PositionAssessment
    val whiteSlightlyBetter = new Glyph(14, "⩲", "White is slightly better") with PositionAssessment
    val blackSlightlyBetter = new Glyph(15, "⩱", "Black is slightly better") with PositionAssessment
    val whiteQuiteBetter = new Glyph(16, "±", "White is quite better") with PositionAssessment
    val blackQuiteBetter = new Glyph(17, "∓", "Black is quite better") with PositionAssessment
    val whiteMuchBetter = new Glyph(18, "+−", "White is much better") with PositionAssessment
    val blackMuchBetter = new Glyph(19, "-+", "Black is much better") with PositionAssessment

    val all = List(equal, unclear, whiteSlightlyBetter, blackSlightlyBetter, whiteQuiteBetter, blackQuiteBetter, whiteMuchBetter, blackMuchBetter)
    val byId = all.map { g => g.id -> g }.toMap
  }

  sealed trait Observation extends Glyph

  object Observation {
    val zugzwang = new Glyph(22, "⨀", "Zugzwang") with Observation
    val development = new Glyph(32, "↑↑", "Development") with Observation
    val initiative = new Glyph(36, "→", "Initiative") with Observation
    val attack = new Glyph(40, "↑", "Attack") with Observation
    val counterplay = new Glyph(132, "⇆", "Counterplay") with Observation
    val timeTrouble = new Glyph(138, "⊕", "Time trouble") with Observation
    val novelty = new Glyph(146, "N", "Novelty") with Observation

    val all = List(zugzwang, development, initiative, attack, counterplay, timeTrouble, novelty)
    val byId = all.map { g => g.id -> g }.toMap
  }
}
