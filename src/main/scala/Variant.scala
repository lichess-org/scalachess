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
  def atomicChess = this == Variant.AtomicChess

  def exotic = !standard

  def pieces: Map[Pos, Piece] = Variant.symmetricRank(
    IndexedSeq(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
  )

  def isValidPromotion(promotion : Option[PromotableRole]) = promotion match {
    case None => true
    case Some(Queen | Rook | Knight | Bishop) => true
    case _ => false
  }

  def validMoves(situation: Situation) : Map[Pos, List[Move]] =  situation.actors collect {
    case actor if actor.moves.nonEmpty => actor.pos -> actor.moves
  } toMap

  def move(situation : Situation, from: Pos, to: Pos, promotion : Option[PromotableRole]): Valid[Move] = {

    // Find the move in the variant specific list of valid moves
    def findMove(from: Pos, to: Pos) = situation.moves get from flatMap (_.find(_.dest == to))

    for {
      actor ← situation.board.actors get from toValid "No piece on " + from
      myActor ← actor.validIf(actor is situation.color, "Not my piece on " + from)
      m1 ← findMove(from,to) toValid "Piece on " + from + " cannot move to " + to
      m2 ← m1 withPromotion promotion toValid "Piece on " + from + " cannot promote to " + promotion
      m3 <- m2 validIf (isValidPromotion(promotion), "Cannot promote to " + promotion + " in this game mode")
    } yield m3
  }

  def staleMate(situation: Situation) : Boolean = !situation.check && situation.moves.isEmpty

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation) : Option[Color] =
    if (situation.checkMate || specialEnd(situation)) Some(!situation.color) else None

  def specialEnd(situation: Situation) = false

  def specialDraw(situation: Situation) = false

  def specialStatemate(situation: Situation) = false

  def drawsOnInsufficientMaterial = true

  def finalizeMove(board: Board): Board = board

  // Some variants, such as atomic chess, give different properties to pieces by replacing them with
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

  }

  case object Antichess extends Variant(
    id = 6,
    key = "antichess",
    name = "Antichess",
    shortName= "Anti",
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
      val capturingMoves = allMoves mapValues (_.filter(_.captures) ) filterNot (_._2.isEmpty)

      if (!capturingMoves.isEmpty) capturingMoves else allMoves
    }

    // In antichess, there is no checkmate condition, and the winner is the current player if they have no legal moves
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
        // No player can win if the only remaining pieces are two bishops of different colours on different coloured
        // diagonals
        case List(act1, act2) =>
          val bothPiecesAreBishops = act1.piece.is(Bishop) && act2.piece.is(Bishop)
          val notSamePlayerColour = (act1.color != act2.color)
          val notOnSameColouredDiagonals = act1.pos.color != act2.pos.color

          bothPiecesAreBishops && notOnSameColouredDiagonals && notSamePlayerColour
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

  case object AtomicChess extends Variant(
    id = 7,
    key = "atomicChess",
    name = "Atomic chess",
    shortName= "Atom",
    title= "Nuke your opponent's king to win."
  ) {

    /** Moves which threaten to explode the opponent's king */
    private def kingThreateningMoves(situation: Situation): Map[Pos,List[Move]] = {

      val moves = for {
        opponentKingPerimeter <- situation.board.kingPosOf(!situation.color) map (_.surroundingPositions)

        kingAttackingMoves = situation.actors map {
          act =>
            // Filter to moves which take a piece next to the king, exploding the king. The player's king cannot
            // capture, however
            act.pos -> act.rawMoves.filter(
              mv => opponentKingPerimeter.contains(mv.dest) && mv.captures && (mv.piece isNot King))
        } filter (!_._2.isEmpty)

      } yield kingAttackingMoves.toMap

      moves getOrElse Map.empty
    }

    override def validMoves(situation: Situation): Map[Pos, List[Move]] = {
      // In atomic chess, the pieces have the same roles as usual
      val usualMoves = super.validMoves(situation)

      /* However, it is illegal for a king to capture as that would result in it exploding. */
      val atomicMoves = for {
        kingPos <- situation.kingPos
        newKingMoves <- usualMoves.get(kingPos) map (_.filterNot(_.captures))
        newMap = usualMoves.updated(kingPos, newKingMoves)
      } yield if (!newKingMoves.isEmpty) newMap else newMap - kingPos // If a pos has no valid moves, we remove it

      val kingSafeMoves = atomicMoves getOrElse usualMoves

      // Additionally, if the player's king is in check they may prioritise exploding the opponent's king over defending
      // their own
      if (!situation.check) kingSafeMoves else kingSafeMoves ++ kingThreateningMoves(situation)
    }

    override def move(situation : Situation, from: Pos, to: Pos, promotion : Option[PromotableRole]) = for {
      m1 <- super.move(situation, from, to, promotion)
      m2 <- explodeSurroundingPieces(m1).success
    } yield m2

    /** If the move captures, we explode the surrounding pieces. Otherwise, nothing explodes. */
    private def explodeSurroundingPieces(move: Move) : Move = {
      if (!move.captures) move
      else {
        val surroundingPositions = move.dest.surroundingPositions
        val afterBoard = move.after
        val destination = move.dest

        val boardPieces = afterBoard.pieces

        // Pawns are immune (for some reason), but all pieces surrounding the captured piece and the capturing piece
        // itself explode
        val piecesToExplode = destination :: surroundingPositions.filter(boardPieces.get(_).fold(false)(_.isNot(Pawn)))
        val afterExplosions = boardPieces -- piecesToExplode

        val newBoard = afterBoard withPieces afterExplosions
        move withAfter newBoard
      }
    }

    /** Since a king may walk into the path of another king, it is more difficult to win when your opponent only has a
      * king left.
      **/
    private def insufficientAtomicWinningMaterial(board: Board) = {
      val whiteActors = board.actorsOf(White)
      val blackActors = board.actorsOf(Black)
      lazy val allActors = board.actors.values.map(_.piece).filter(_ isNot King)

      if (whiteActors.size != 1 && blackActors.size != 1) false
      else {
        // You can mate with a king and a rook or queen, but not a king and a bishop or knight
        allActors.size == 1 && allActors.exists(_ isMinor)
      }
    }

    override def specialDraw(situation: Situation) = {
      // Bishops on opposite coloured squares can never capture each other to cause a king to explode
      // and a rook and a king vs a king is not winnable
      val board = situation.board
      InsufficientMatingMaterial.bishopsOnDifferentColor(board) || insufficientAtomicWinningMaterial(board)
    }

    // On insufficient mating material, a win may still be achieved by exploding a piece next to a king
    override def drawsOnInsufficientMaterial = false

    /** Atomic chess has a special end where the king has been killed by exploding with an adjacent captured piece */
    override def specialEnd(situation: Situation) = situation.kingPos.isEmpty
  }

  val all = List(Standard, Chess960, FromPosition, KingOfTheHill, ThreeCheck, Antichess, AtomicChess)
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
