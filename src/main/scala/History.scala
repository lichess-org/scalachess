package chess

import Pos.posAt

case class History(
    lastMove: Option[(Pos, Pos)] = None,
    positionHashes: List[String] = Nil,
    castles: Castles = Castles.all) {

  def lastMoveString: Option[String] = lastMove map {
    case (p1, p2) ⇒ p1.toString + p2.toString
  }

  def threefoldRepetition: Boolean = positionHashes.size > 6 && {
    positionHashes.headOption map { hash ⇒
      positionHashes.count(_ == hash) >= 3
    } getOrElse false
  }

  def canCastle(color: Color) = new {
    def on(side: Side): Boolean = castles can color on side
    def any = (castles can color).any
  }

  def withoutCastles(color: Color) = copy(castles = castles without color)

  def withoutAnyCastles = copy(castles = Castles.none)

  def withoutCastle(color: Color, side: Side) = copy(castles = castles.without(color, side))

  def withNewPositionHash(hash: String): History =
    copy(positionHashes = positionHashesWith(hash))

  def positionHashesWith(hash: String): List[String] =
    (hash take History.hashSize) :: positionHashes

  def withLastMove(orig: Pos, dest: Pos) = copy(
    lastMove = Some((orig, dest))
  )
}

object History {

  val hashSize = 5

  val MoveString = """^([a-h][1-8])([a-h][1-8])$""".r

  def apply(
    lastMove: Option[(Pos, Pos)],
    positionHashes: String,
    castles: Castles): History = new History(
    lastMove = lastMove,
    castles = castles,
    positionHashes = positionHashes grouped hashSize toList)

  def apply(
    lastMove: Option[String], // a2a4
    positionHashes: String,
    castles: String): History = apply(
    lastMove = lastMove flatMap {
      case MoveString(a, b) ⇒ for (o ← posAt(a); d ← posAt(b)) yield (o, d)
      case _                ⇒ None
    },
    positionHashes = positionHashes,
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
