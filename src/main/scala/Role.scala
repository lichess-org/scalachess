package chess

import Pos._

sealed trait Role {
  val forsyth: Char
  lazy val pgn: Char = forsyth.toUpper
  lazy val name = toString.toLowerCase
  val attacker: Boolean = true
  val projection: Boolean = false
  def dirs: Directions
}
sealed trait PromotableRole extends Role
sealed trait Projection extends Role {
  def dir(from: Pos, to: Pos): Option[Direction]
  override val projection = true
}

case object King extends Role {
  val forsyth = 'k'
  val dirs: Directions = Queen.dirs
  override val attacker = false
}
case object Queen extends PromotableRole with Projection {
  val forsyth = 'q'
  val dirs: Directions = Rook.dirs ::: Bishop.dirs
  def dir(from: Pos, to: Pos) = Rook.dir(from, to) orElse Bishop.dir(from, to)
}
case object Rook extends PromotableRole with Projection {
  val forsyth = 'r'
  val dirs: Directions = List(_.up, _.down, _.left, _.right)
  def dir(from: Pos, to: Pos) = if (to ?| from) Some(
    if (to ?^ from) (_.up) else (_.down)
  )
  else if (to ?- from) Some(
    if (to ?< from) (_.left) else (_.right)
  )
  else None
}
case object Bishop extends PromotableRole with Projection {
  val forsyth = 'b'
  val dirs: Directions = List(_.upLeft, _.upRight, _.downLeft, _.downRight)
  def dir(from: Pos, to: Pos) = if (to onSameDiagonal from) Some(
    if (to ?^ from) {
      if (to ?< from) (_.upLeft) else (_.upRight)
    } else {
      if (to ?< from) (_.downLeft) else (_.downRight)
    }
  ) else None
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
}
case object Pawn extends Role {
  val forsyth = 'p'
  val dirs: Directions = Nil
}

object Role {

  val all: List[Role] = List(King, Queen, Rook, Bishop, Knight, Pawn)
  val allPromotable: List[PromotableRole] = List(Queen, Rook, Bishop, Knight)
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
