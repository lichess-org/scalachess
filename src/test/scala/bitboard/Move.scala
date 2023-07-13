package chess
package bitboard

// case Capture(from: square, to: square, role: Role, capture: Role)???
// case Drop/Put(role: Role, to: square)
enum Move(val from: Square, val to: Square, val role: Role):
  case Normal(override val from: Square, override val to: Square, override val role: Role, capture: Boolean)
      extends Move(from, to, role)
  case Castle(override val from: Square, override val to: Square)    extends Move(from, to, King)
  case EnPassant(override val from: Square, override val to: Square) extends Move(from, to, Pawn)
  case Promotion(override val from: Square, override val to: Square, promoted: Role, capture: Boolean)
      extends Move(from, to, Pawn)

  def isCapture: Boolean =
    this match
      case EnPassant(_, _)             => true
      case Castle(_, _)                => false
      case Promotion(_, _, _, capture) => capture
      case Normal(_, _, _, capture)    => capture

  def isHalfMove: Boolean =
    role != Pawn && !isCapture

  extension (square: Square)
    def uci: String =
      val f = square.file.char
      val r = square.rank.char
      s"$f$r"

  def uci: String =
    this match
      case Promotion(from, to, role, _) => s"${from.uci}${to.uci}${role.forsyth}"
      case Castle(from, to) =>
        val k = from.withFile(if to < from then File.C else File.G)
        s"${from.uci}${k.uci}"
      case _ => s"${from.uci}${to.uci}"
