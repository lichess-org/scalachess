package chess
package pattern

enum Pattern:
  case fork
  case doubleCheck
  case xRay

case class PatternAt(pattern: Pattern, at: Ply)

object PatternAt:

  def findAll(replay: Replay): List[PatternAt] =
    Nil
