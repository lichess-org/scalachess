package chess

import scala.util.Random

import Pos.posAt

sealed abstract class Variant(
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String) {

  def standard = this == Variant.Standard
  def chess960 = this == Variant.Chess960
  def kingOfTheHill = this == Variant.KingOfTheHill
  def threeCheck = this == Variant.ThreeCheck

  def exotic = !standard

  def pieces: Map[Pos, Piece] = Variant.symmetricRank(
    IndexedSeq(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
  )

  def specialEnd(situation: Situation) = false

  def drawsOnInsufficientMaterial = true

  def finalizeMove(board: Board): Board = board

  override def toString = name
}

object Variant {

  case object Standard extends Variant(
    id = 1,
    key = "standard",
    name = "Standard",
    shortName = "STD",
    title = "Standard rules of chess (FIDE)")

  case object Chess960 extends Variant(
    id = 2,
    key = "chess960",
    name = "Chess960",
    shortName = "960",
    title = "Starting position of the home rank pieces is randomized") {

    override def pieces = symmetricRank {
      val size = 8
      type Rank = IndexedSeq[Option[Role]]
      def ?(max: Int) = Random nextInt max
      def empty(rank: Rank, skip: Int): Option[Int] = {
        1 to size find (x => (rank take x count (_.isEmpty)) == skip + 1)
      } map (_ - 1)
      def update(rank: Rank, role: Role)(x: Int): Rank =
        rank.updated(x, role.some)
      def place(rank: Rank, role: Role, x: Int): Option[Rank] =
        empty(rank, x) map update(rank, role)
      val bishops: Rank =
        IndexedSeq.fill(8)(none[Role])
          .updated(2 * ?(4), Bishop.some) // place first bishop
          .updated(2 * ?(4) + 1, Bishop.some) // place second bishop

      val rank = for {
        a1 ← bishops.some
        a2 ← place(a1, Queen, ?(6))
        a3 ← place(a2, Knight, ?(5))
        a4 ← place(a3, Knight, ?(4))
        a5 ← place(a4, Rook, 0)
        a6 ← place(a5, King, 0)
        a7 ← place(a6, Rook, 0)
      } yield a7

      rank.err("WTF").flatten
    }
  }

  case object FromPosition extends Variant(
    id = 3,
    key = "fromPosition",
    name = "From Position",
    shortName = "FEN",
    title = "Custom starting position")

  case object KingOfTheHill extends Variant(
    id = 4,
    key = "kingOfTheHill",
    name = "King of the Hill",
    shortName = "KotH",
    title = "Bring your king to the center to win the game") {

    private val center = Set(Pos.D4, Pos.D5, Pos.E4, Pos.E5)

    override def specialEnd(situation: Situation) =
      situation.board.kingPosOf(!situation.color) exists center.contains

    override def drawsOnInsufficientMaterial = false
  }

  case object ThreeCheck extends Variant(
    id = 5,
    key = "threeCheck",
    name = "Three-check",
    shortName = "3+",
    title = "Check your opponent 3 times to win the game") {

    override def finalizeMove(board: Board) = board updateHistory {
      _.withCheck(Color.White, board.checkWhite).withCheck(Color.Black, board.checkBlack)
    }

    override def specialEnd(situation: Situation) = situation.check && {
      val checks = situation.board.history.checkCount
      situation.color.fold(checks.white, checks.black) >= 3
    }
  }

  val all = List(Standard, Chess960, FromPosition, KingOfTheHill, ThreeCheck)
  val byId = all map { v => (v.id, v) } toMap

  val default = Standard

  def apply(id: Int): Option[Variant] = byId get id
  def orDefault(id: Int): Variant = apply(id) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  private def symmetricRank(rank: IndexedSeq[Role]): Map[Pos, Piece] =
    (for (y ← Seq(1, 2, 7, 8); x ← 1 to 8) yield {
      posAt(x, y) map { pos =>
        (pos, y match {
          case 1 => White - rank(x - 1)
          case 2 => White.pawn
          case 7 => Black.pawn
          case 8 => Black - rank(x - 1)
        })
      }
    }).flatten.toMap
}
