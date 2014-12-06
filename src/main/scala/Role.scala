package chess

import Pos._

sealed trait Role {
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val pgn: Char = forsythUpper
  lazy val name = toString.toLowerCase
  val attacker: Boolean = true
  val projection: Boolean
  val dirs: Directions
  def dir(from: Pos, to: Pos): Option[Direction]
}
sealed trait PromotableRole extends Role

case object King extends Role {
  val forsyth = 'k'
  val dirs: Directions = Queen.dirs
  def dir(from: Pos, to: Pos) = None
  override val attacker = false
  val projection = false
}

/**
 * King for antichess variant where there is no check or checkmate. Additionally, a pawn is promotable to a king.
 */
case object SuicideKing extends PromotableRole {
  val forsyth = 'k'
  val dirs: Directions = Queen.dirs
  def dir(from: Pos, to: Pos) = None
  val projection = false
}

case object Queen extends PromotableRole {
  val forsyth = 'q'
  val dirs: Directions = Rook.dirs ::: Bishop.dirs
  def dir(from: Pos, to: Pos) = Rook.dir(from, to) orElse Bishop.dir(from, to)
  val projection = true
}
case object Rook extends PromotableRole {
  val forsyth = 'r'
  val dirs: Directions = List(_.up, _.down, _.left, _.right)
  def dir(from: Pos, to: Pos) = if (to ?| from) Some(
    if (to ?^ from) (_.up) else (_.down)
  )
  else if (to ?- from) Some(
    if (to ?< from) (_.left) else (_.right)
  )
  else None
  val projection = true
}
case object Bishop extends PromotableRole {
  val forsyth = 'b'
  val dirs: Directions = List(_.upLeft, _.upRight, _.downLeft, _.downRight)
  def dir(from: Pos, to: Pos) = if (to onSameDiagonal from) Some(
    if (to ?^ from) {
      if (to ?< from) (_.upLeft) else (_.upRight)
    }
    else {
      if (to ?< from) (_.downLeft) else (_.downRight)
    }
  )
  else None
  val projection = true
}
case object Knight extends PromotableRole {
  val forsyth = 'n'
  val dirs: Directions = List(
    _.up flatMap (_.upLeft),
    _.up flatMap (_.upRight),
    _.left flatMap (_.upLeft),
    _.left flatMap (_.downLeft),
    _.right flatMap (_.upRight),
    _.right flatMap (_.downRight),
    _.down flatMap (_.downLeft),
    _.down flatMap (_.downRight))
  def dir(from: Pos, to: Pos) = None
  val projection = false
}
case object Pawn extends Role {
  val forsyth = 'p'
  val dirs: Directions = Nil
  def dir(from: Pos, to: Pos) = None
  val projection = false
}

object Role {

  val all: List[Role] = List(King, Queen, Rook, Bishop, Knight, Pawn)
  val allPromotable: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, SuicideKing)
  val allByForsyth: Map[Char, Role] = all map { r => (r.forsyth, r) } toMap
  val allByPgn: Map[Char, Role] = all map { r => (r.pgn, r) } toMap
  val allPromotableByName: Map[String, PromotableRole] =
    allPromotable map { r => (r.toString, r) } toMap
  val allPromotableByForsyth: Map[Char, PromotableRole] =
    allPromotable map { r => (r.forsyth, r) } toMap
  val allPromotableByPgn: Map[Char, PromotableRole] =
    allPromotable map { r => (r.pgn, r) } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable
}
