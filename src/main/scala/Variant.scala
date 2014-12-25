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
  def antichess = this == Variant.Antichess

  def exotic = !standard

  def pieces: Map[Pos, Piece] = Variant.symmetricRank(
    IndexedSeq(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
  )

  def isValidPromotion(promotion : Option[PromotableRole]) = promotion match {
    case None => true
    case Some(Queen | Rook | Knight | Bishop) => true
    case _ => false
  }

  def validMoves(situation: Situation) =  situation.actors collect {
    case actor if actor.moves.nonEmpty => actor.pos -> actor.moves
  } toMap

  def move(situation : Situation, from: Pos, to: Pos, promotion : Option[PromotableRole]): Valid[Move] = for {
    actor ← situation.board.actors get from toValid "No piece on " + from
    myActor ← actor.validIf(actor is situation.color, "Not my piece on " + from)
    m1 ← myActor.moves find (_.dest == to) toValid "Piece on " + from + " cannot move to " + to
    m2 ← m1 withPromotion promotion toValid "Piece on " + from + " cannot promote to " + promotion
    m3 <- m2 validIf (isValidPromotion(promotion), "Cannot promote to " + promotion + " in this game mode")
  } yield m3

  def staleMate(situation: Situation) : Boolean = !situation.check && situation.moves.isEmpty

  def winner(situation: Situation) : Option[Color] =  if (situation.checkMate) Some(!situation.color) else None

  def specialEnd(situation: Situation) = false

  def specialDraw(situation: Situation) = false

  def specialStatemate(situation: Situation) = false

  def drawsOnInsufficientMaterial = true

  def finalizeMove(board: Board): Board = board

  // Some variants, such as kamikaze chess, give different properties to pieces by replacing them with
  // different piece objects
  def convertPiecesFromStandard(originalPieces : PieceMap) : PieceMap = originalPieces

  def valid(board : Board, strict: Boolean) = {
    Color.all map board.rolesOf forall { roles =>
      ((roles count (_ == King)) == 1) :: {
        if (strict) List((roles count (_ == Pawn)) <= 8, roles.size <= 16) else Nil
      } forall identity
    }
  }

  def roles = List(Rook, Knight, King, Bishop, King, Queen, Pawn)

  def promotableRoles : List[PromotableRole] = List(Queen, Rook, Bishop, Knight)

  def rolesByForsyth: Map[Char, Role] = this.roles map { r => (r.forsyth, r) } toMap

  def rolesByPgn: Map[Char, Role] = this.roles map { r => (r.pgn, r) } toMap

  def rolesPromotableByPgn: Map[Char, PromotableRole] =
    promotableRoles map { r => (r.pgn, r) } toMap

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

    override def winner(situation: Situation) = if (specialEnd(situation)) Some(!situation.color) else None

  }

  case object Antichess extends Variant(
    id = 6,
    key = "antichess",
    name = "Antichess",
    shortName= "ant",
    title= "Lose all your pieces to win the game"
  ) {

    override def pieces = {
      // In this chess variant, the king can ignore check and be captured, so we replace the normal king with the
      // antichess king
      convertPiecesFromStandard(super.pieces)
    }

    // In this variant, a player must capture if a capturing move is available
    override def validMoves(situation: Situation) = {
      val allMoves = super.validMoves(situation)
      val capturingMoves = super.validMoves(situation) mapValues (_.filter(_.captures) ) filterNot (_._2.isEmpty)

      if (!capturingMoves.isEmpty) capturingMoves else allMoves
    }

    override def move(situation : Situation, from: Pos, to: Pos, promotion : Option[PromotableRole]) = for {
    // We inherit the standard rules, such as where peices may move
      m1 <- super.move(situation, from, to, promotion)

      // However, in antichess, the player may only move without capturing if no capturing moves are available.
      m2 <- m1 validIf (m1.captures || !situation.playerCanCapture, "there are capturing moves available")

    } yield m2

    override def staleMate(situation: Situation) : Boolean = specialDraw(situation)

    override def winner (situation: Situation): Option[Color] = if (specialEnd(situation)) Some(situation.color) else None

    override def specialEnd(situation: Situation) = {
      // The game ends with a win when one player manages to lose all their pieces or is in stalemate
      situation.board.actorsOf(situation.color).isEmpty || situation.moves.isEmpty
    }

    // This mode has no checkmates
    override def drawsOnInsufficientMaterial = false

    override def specialDraw(situation: Situation) = {
      val actors = situation.board.actors
      if (actors.size != 2) false
      else actors.values.toList match {
        // No player can win if the only remaining pieces are two bishops of different colours
        case List(act1, act2) => (act1.color != act2.color) && act1.piece.is(Bishop) && act2.piece.is(Bishop)
        case _ => false
      }
    }

    override def convertPiecesFromStandard(pieces : PieceMap) : PieceMap = {
      pieces.mapValues {
        case Piece (color, King) => Piece (color, Antiking)
        case x => x
      }
    }

    override def valid (board: Board, strict: Boolean) = {
      // This variant cannot work with a 'normal' king as it assumes an AntiKing

      board.pieces.values.find(_.is(King)).isEmpty &&  {
        Color.all map board.rolesOf forall { roles =>
          (if (strict) List((roles count (_ == Pawn)) <= 8, roles.size <= 16) else Nil) forall identity
        }
      }
    }

    // In this game variant, a king is a valid promotion
    override def isValidPromotion(promotion : Option[PromotableRole]) = promotion match {
      case None => true
      case Some(Queen | Rook | Knight | Bishop | Antiking) => true
      case _ => false
    }

    override def roles = List(Rook, Knight, Antiking, Bishop, Queen, Pawn)

    override def promotableRoles : List[PromotableRole] = List(Queen, Rook, Bishop, Knight, Antiking)

  }

  val all = List(Standard, Chess960, FromPosition, KingOfTheHill, ThreeCheck, Antichess)
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
