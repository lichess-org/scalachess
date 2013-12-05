package chess

import Pos.posAt

case class History(
    lastMove: Option[(Pos, Pos)] = None,
    positionHashes: PositionHash = Array(),
    castles: Castles = Castles.all) {

  def lastMoveString: Option[String] = lastMove map {
    case (p1, p2) ⇒ p1.toString + p2.toString
  }

  def threefoldRepetition: Boolean = positionHashes.size > 12 && {
    val positions = (positionHashes grouped 2).toList
    positions.headOption map { hash ⇒
      positions.count(_ == hash) >= 3
    } getOrElse false
  }

  def canCastle(color: Color) = new {
    def on(side: Side): Boolean = castles can color on side
    def any = (castles can color).any
  }

  def withoutCastles(color: Color) = copy(castles = castles without color)

  def withoutAnyCastles = copy(castles = Castles.none)

  def withoutCastle(color: Color, side: Side) = copy(castles = castles.without(color, side))

  def withNewPositionHash(hash: PositionHash): History =
    copy(positionHashes = positionHashesWith(hash))

  def positionHashesWith(hash: PositionHash): PositionHash =
    hash ++ positionHashes

  def withLastMove(orig: Pos, dest: Pos) = copy(
    lastMove = Some((orig, dest))
  )
}

object History {

  val MoveString = """^([a-h][1-8])([a-h][1-8])$""".r

  def make(
    lastMove: Option[(Pos, Pos)],
    positionHashes: PositionHash,
    castles: Castles): History = new History(
    lastMove = lastMove,
    castles = castles,
    positionHashes = positionHashes)

  def make(
    lastMove: Option[String], // a2a4
    castles: String): History = make(
    lastMove = lastMove flatMap {
      case MoveString(a, b) ⇒ for (o ← posAt(a); d ← posAt(b)) yield (o, d)
      case _                ⇒ None
    },
    positionHashes = Array(),
    castles = Castles(castles))

  def castle(color: Color, kingSide: Boolean, queenSide: Boolean) =
    History().copy(
      castles = color match {
        case White ⇒ Castles.init.copy(
          whiteKingSide = kingSide,
          whiteQueenSide = queenSide)
        case Black ⇒ Castles.init.copy(
          blackKingSide = kingSide,
          blackQueenSide = queenSide)
      })

  def noCastle = History() withoutCastles White withoutCastles Black
}
