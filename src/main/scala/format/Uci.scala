package chess
package format

import cats.data.Validated
import cats.syntax.all.*

sealed trait Uci:

  def uci: String
  def chars: String

  def origDest: (Square, Square)

  def apply(situation: Situation): Validated[ErrorStr, MoveOrDrop]

object Uci:

  case class Move(
      orig: Square,
      dest: Square,
      promotion: Option[PromotableRole] = None
  ) extends Uci:

    def keys = s"${orig.key}${dest.key}"
    def uci  = s"$keys$promotionString"

    def charKeys = s"${orig.asChar}${dest.asChar}"
    def chars    = s"$charKeys$promotionString"

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = orig -> dest

    def apply(situation: Situation) = situation.move(orig, dest, promotion)

    override def toString = s"Move(${orig.key}${dest.key}${promotion.fold("")(_.forsyth)})"

  object Move:

    def apply(move: String): Option[Move] =
      for {
        orig <- Square.fromKey(move take 2)
        dest <- Square.fromKey(move.slice(2, 4))
        promotion = move lift 4 flatMap Role.promotable
      } yield Move(orig, dest, promotion)

    def fromChars(move: String) =
      for {
        orig <- move.headOption flatMap { Square.fromChar(_) }
        dest <- move lift 1 flatMap { Square.fromChar(_) }
        promotion = move lift 2 flatMap { Role.promotable(_) }
      } yield Move(orig, dest, promotion)

    def fromStrings(origS: String, destS: String, promS: Option[String]) =
      for {
        orig <- Square.fromKey(origS)
        dest <- Square.fromKey(destS)
        promotion = Role promotable promS
      } yield Move(orig, dest, promotion)

  case class Drop(role: Role, square: Square) extends Uci:

    def uci = s"${role.pgn}@${square.key}"

    def chars = s"${role.pgn}@${square.asChar}"

    def origDest = square -> square

    def apply(situation: Situation) = situation.drop(role, square)

  object Drop:

    def fromChars(move: String) = for {
      role   <- move.headOption flatMap Role.allByPgn.get
      square <- move lift 2 flatMap { Square.fromChar(_) }
    } yield Uci.Drop(role, square)

    def fromStrings(roleS: String, posS: String) =
      for {
        role   <- Role.allByName get roleS
        square <- Square.fromKey(posS)
      } yield Drop(role, square)

  case class WithSan(uci: Uci, san: pgn.SanStr)

  def apply(move: chess.Move) = Uci.Move(move.orig, move.dest, move.promotion)

  def apply(drop: chess.Drop) = Uci.Drop(drop.piece.role, drop.square)

  def apply(move: String): Option[Uci] =
    if (move lift 1 contains '@') for {
      role   <- move.headOption flatMap Role.allByPgn.get
      square <- Square.fromKey(move.slice(2, 4))
    } yield Uci.Drop(role, square)
    else Uci.Move(move)

  def fromChars(move: String): Option[Uci] =
    if (move lift 1 contains '@') Uci.Drop.fromChars(move)
    else Uci.Move.fromChars(move)

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListChars(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(fromChars).sequence

  def writeListChars(moves: List[Uci]): String =
    moves.map(_.chars) mkString " "
