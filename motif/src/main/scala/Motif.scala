package chess
package motif

enum Motif:
  case fork
  case doubleCheck
  case xRay

case class MotifAt(motif: Motif, at: Ply)

object MotifAt:

  def findAll(replay: Replay): List[MotifAt] =
    Nil
