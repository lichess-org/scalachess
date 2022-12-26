package chess
package bitboard

// TODO should capture be only Boolean? what is the benefit of Role?
// case Capture(from: Pos, to: Pos, role: Role, capture: Role)???
// case Drop/Put(role: Role, to: Pos)
enum Move(val from: Pos, val to: Pos, val role: Role):
  case Normal(override val from: Pos, override val to: Pos, override val role: Role, capture: Boolean)
      extends Move(from, to, role)
  case Castle(override val from: Pos, override val to: Pos)    extends Move(from, to, King)
  case EnPassant(override val from: Pos, override val to: Pos) extends Move(from, to, Pawn)
  case Promotion(override val from: Pos, override val to: Pos, promoted: Role, capture: Boolean)
      extends Move(from, to, Pawn)

  def isCapture: Boolean =
    this match
      case EnPassant(_, _)             => true
      case Castle(_, _)                => false
      case Promotion(_, _, _, capture) => capture
      case Normal(_, _, _, capture)    => capture

  def isHalfMove: Boolean =
    role != Pawn && !isCapture

  extension (pos: Pos)
    def uci: String =
      val f = pos.file.char
      val r = pos.rank.char
      s"$f$r"

  def uci: String =
    this match
      case Promotion(from, to, role, _) => s"${from.uci}${to.uci}${role.forsyth}"
      case Castle(from, to) => {
        val k = from.withFile(if to < from then File.C else File.G)
        s"${from.uci}${k.uci}"
      }
      case _ => s"${from.uci}${to.uci}"
