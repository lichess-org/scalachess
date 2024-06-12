package chess
package motif

enum Motif:
  case attackOnF2F7
  case attraction
  case captureDefender
  case clearance
  case deflection
  case discoveredAttack
  case doubleCheck
  case fork(by: Color, orig: Square, dests: Square)
  case hangingPiece
  case interference
  case intermezzo
  case overloading
  case pin
  case quietMove
  case sacrifice(by: Color)
  case skewer
  case trappedPiece
  case xRayAttack

case class MotifAt(motif: Motif, at: Ply)

object MotifAt:

  def findAll(replay: Replay): List[MotifAt] =
    Nil
