package chess
package format

import cats.data.Validated
import cats.implicits.*

sealed trait Uci:

  def uci: String
  def chars: String

  def origDest: (Pos, Pos)

  def apply(situation: Situation): Validated[String, MoveOrDrop]

object Uci:

  case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None
  ) extends Uci:

    def keys = s"${orig.key}${dest.key}"
    def uci  = s"$keys$promotionString"

    def charKeys = s"${orig.toChar}${dest.toChar}"
    def chars    = s"$charKeys$promotionString"

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = orig -> dest

    def apply(situation: Situation) = situation.move(orig, dest, promotion) map Left.apply

  object Move:

    def apply(move: String): Option[Move] =
      for {
        orig <- Pos.fromKey(move take 2)
        dest <- Pos.fromKey(move.slice(2, 4))
        promotion = move lift 4 flatMap Role.promotable
      } yield Move(orig, dest, promotion)

    def fromChars(move: String) =
      for {
        orig <- move.headOption flatMap { Pos.fromChar(_) }
        dest <- move lift 1 flatMap { Pos.fromChar(_) }
        promotion = move lift 2 flatMap { Role.promotable(_) }
      } yield Move(orig, dest, promotion)

    def fromStrings(origS: String, destS: String, promS: Option[String]) =
      for {
        orig <- Pos.fromKey(origS)
        dest <- Pos.fromKey(destS)
        promotion = Role promotable promS
      } yield Move(orig, dest, promotion)

  case class Drop(role: Role, pos: Pos) extends Uci:

    def uci = s"${role.pgn}@${pos.key}"

    def chars = s"${role.pgn}@${pos.toChar}"

    def origDest = pos -> pos

    def apply(situation: Situation) = situation.drop(role, pos) map Right.apply

  object Drop:

    def fromChars(move: String) = for {
      role <- move.headOption flatMap Role.allByPgn.get
      pos  <- move lift 2 flatMap { Pos.fromChar(_) }
    } yield Uci.Drop(role, pos)

    def fromStrings(roleS: String, posS: String) =
      for {
        role <- Role.allByName get roleS
        pos  <- Pos.fromKey(posS)
      } yield Drop(role, pos)

  case class WithSan(uci: Uci, san: String)

  def apply(move: chess.Move) = Uci.Move(move.orig, move.dest, move.promotion)

  def apply(drop: chess.Drop) = Uci.Drop(drop.piece.role, drop.pos)

  def apply(move: String): Option[Uci] =
    if (move lift 1 contains '@') for {
      role <- move.headOption flatMap Role.allByPgn.get
      pos  <- Pos.fromKey(move.slice(2, 4))
    } yield Uci.Drop(role, pos)
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
