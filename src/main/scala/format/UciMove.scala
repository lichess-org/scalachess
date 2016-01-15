package chess
package format

sealed trait Uci {

  def uci: String
  def piotr: String
}

object Uci
    extends scalaz.std.OptionInstances
    with scalaz.syntax.ToTraverseOps {

  case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None) extends Uci {

    def keys = orig.key + dest.key
    def uci = keys + promotionString

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr = keysPiotr + promotionString

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = orig -> dest
  }

  case class Drop(role: Role, pos: Pos) extends Uci {

    def uci = s"${role.pgn}@${pos.key}"

    def piotr = s"${role.pgn}@${pos.piotrStr}"
  }

  def apply(move: Move): Uci = Uci.Move(move.orig, move.dest, move.promotion)

  def apply(move: String): Option[Uci] =
    if (move lift 1 contains "@") for {
      role ← move.headOption flatMap Role.allByPgn.get
      pos ← Pos.posAt(move drop 2 take 2)
    } yield Uci.Drop(role, pos)
    else for {
      orig ← Pos.posAt(move take 2)
      dest ← Pos.posAt(move drop 2 take 2)
      promotion = move lift 4 flatMap Role.promotable
    } yield Uci.Move(orig, dest, promotion)

  def piotr(move: String): Option[Uci] =
    if (move lift 1 contains "@") for {
      role ← move.headOption flatMap Role.allByPgn.get
      pos ← move lift 2 flatMap Pos.piotr
    } yield Uci.Drop(role, pos)
    else for {
      orig ← move.headOption flatMap Pos.piotr
      dest ← move lift 1 flatMap Pos.piotr
      promotion = move lift 2 flatMap Role.promotable
    } yield Uci.Move(orig, dest, promotion)

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci.Move]): String =
    moves.map(_.piotr) mkString " "
}
