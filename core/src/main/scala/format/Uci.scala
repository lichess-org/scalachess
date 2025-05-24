package chess
package format

import cats.syntax.all.*

sealed trait Uci extends Moveable:

  def uci: String
  def chars: String

  def origDest: (Square, Square)

  def rawString: Option[String] = Some(uci)

object Uci:

  case class Move(
      orig: Square,
      dest: Square,
      promotion: Option[PromotableRole] = None
  ) extends Uci:

    def keys: String = s"${orig.key}${dest.key}"
    def uci: String  = s"$keys$promotionString"

    def charKeys: String = s"${orig.asChar}${dest.asChar}"
    def chars: String    = s"$charKeys$promotionString"

    def promotionString: String = promotion.fold("")(_.forsyth.toString)

    def origDest: (Square, Square) = orig -> dest

    def apply(position: Position): Either[ErrorStr, MoveOrDrop] = position.move(orig, dest, promotion)

    override def toString = s"Move(${orig.key}${dest.key}${promotion.fold("")(_.forsyth)})"

  object Move:

    def apply(move: String): Option[Move] = for
      orig <- Square.fromKey(move.take(2))
      dest <- Square.fromKey(move.slice(2, 4))
      promotion = move.lift(4).flatMap(Role.promotable)
    yield Move(orig, dest, promotion)

    def fromChars(move: String): Option[Move] = for
      orig <- move.headOption.flatMap(Square.fromChar)
      dest <- move.lift(1).flatMap(Square.fromChar)
      promotion = move.lift(2).flatMap(Role.promotable)
    yield Move(orig, dest, promotion)

    def fromStrings(origS: String, destS: String, promS: Option[String]): Option[Move] = for
      orig <- Square.fromKey(origS)
      dest <- Square.fromKey(destS)
      promotion = Role.promotable(promS)
    yield Move(orig, dest, promotion)

  case class Drop(role: Role, square: Square) extends Uci:

    def uci: String = s"${role.pgn}@${square.key}"

    def chars: String = s"${role.pgn}@${square.asChar}"

    def origDest: (Square, Square) = square -> square

    def apply(position: Position): Either[ErrorStr, MoveOrDrop] = position.drop(role, square)

  object Drop:

    def fromChars(move: String): Option[Drop] = for
      role   <- move.headOption.flatMap(Role.allByPgn.get)
      square <- move.lift(2).flatMap(Square.fromChar)
    yield Uci.Drop(role, square)

    def fromStrings(roleS: String, posS: String): Option[Drop] = for
      role   <- Role.allByName.get(roleS)
      square <- Square.fromKey(posS)
    yield Drop(role, square)

  case class WithSan(uci: Uci, san: pgn.SanStr)

  def apply(move: chess.Move): Move = Uci.Move(move.orig, move.dest, move.promotion)

  def apply(drop: chess.Drop): Drop = Uci.Drop(drop.piece.role, drop.square)

  def apply(move: String): Option[Uci] =
    if move.lift(1).contains('@') then
      for
        role   <- move.headOption.flatMap(Role.allByPgn.get)
        square <- Square.fromKey(move.slice(2, 4))
      yield Uci.Drop(role, square)
    else Uci.Move(move)

  def fromChars(move: String): Option[Uci] =
    if move.lift(1).contains('@') then Uci.Drop.fromChars(move)
    else Uci.Move.fromChars(move)

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.traverse(apply)

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci).mkString(" ")

  def readListChars(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.traverse(fromChars)

  def writeListChars(moves: List[Uci]): String =
    moves.map(_.chars).mkString(" ")
