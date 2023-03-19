package chess
package variant

import chess.format.EpdFen
import chess.bitboard.Bitboard
import chess.bitboard.Bitboard.*
import cats.Functor
import cats.syntax.all.*

case object Horde
    extends Variant(
      id = Variant.Id(8),
      key = Variant.LilaKey("horde"),
      uciKey = Variant.UciKey("horde"),
      name = "Horde",
      shortName = "Horde",
      title = "Destroy the horde to win!",
      standardInitialPosition = false
    ):

  /** In Horde chess white advances against black with a horde of pawns.
    */
  lazy val pieces: Map[Pos, Piece] =
    val whitePawnsHorde = for {
      x <- File.all
      y <- Rank.all.take(4)
    } yield (Pos(x, y) -> White.pawn)
    val frontPawns  = List(Pos.B5, Pos.C5, Pos.F5, Pos.G5).map { _ -> White.pawn }
    val blackPawns  = File.all.map { Pos(_, Rank.Seventh) -> Black.pawn }
    val blackPieces = File.all.map { x => Pos(x, Rank.Eighth) -> (Black - backRank(x.index)) }
    whitePawnsHorde ++ frontPawns ++ blackPawns ++ blackPieces toMap

  override val castles = Castles("kq")

  override val initialFen = EpdFen(
    "rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1"
  )

  def validMoves(situation: Situation): List[Move] =
    import situation.{ genEnPassant, genNonKing, isWhiteTurn, us, board }
    if isWhiteTurn then genEnPassant(us & board.pawns) ++ genNonKing(~us & ~board.kings)
    else Standard.validMoves(situation)

  override def valid(board: Board, strict: Boolean) =
    board.kingOf(White).isEmpty && validSide(board, strict)(Black) && !pawnsOnPromotionRank(board, White)

  /** The game has a special end condition when black manages to capture all of white's pawns */
  override def specialEnd(situation: Situation) =
    situation.board.white.isEmpty

  /** Any vs K + any where horde is stalemated and only king can move is a fortress draw
    * This does not consider imminent fortresses such as 8/p7/P7/8/8/P7/8/k7 b - -
    * nor does it consider contrived fortresses such as b7/pk6/P7/P7/8/8/8/8 b - -
    */
  private def hordeClosedPosition(situation: Situation): Boolean =
    val hordePos = situation.board.occupation(Color.white) // may include promoted pieces
    val mateInOne =
      hordePos.sizeIs == 1 && hordePos.forall(pos => pieceThreatened(situation.board, Color.black, pos))
    !mateInOne && {
      if situation.isWhiteTurn then situation.legalMoves.isEmpty
      else
        val legalMoves = validMoves(situation)
        legalMoves.filter(_.piece.role != King).isEmpty &&
        legalMoves.filter(_.piece.role == King).forall(move => validMoves(move.situationAfter).isEmpty)
    }

  /** In horde chess, black can win unless a fortress stalemate is unavoidable.
    *  Auto-drawing the game should almost never happen, but it did in https://lichess.org/xQ2RsU8N#121
    */
  override def isInsufficientMaterial(board: Board) =
    Color.all.forall(color => hordeClosedPosition(board.situationOf(color)))

  /** In horde chess, the horde cannot win on * v K or [BN]{2} v K or just one piece
    * since they lack a king for checkmate support.
    * Technically there are some positions where stalemate is unavoidable which
    * this method does not detect; however, such are trivial to premove.
    */
  override def opponentHasInsufficientMaterial(situation: Situation): Boolean =
    val board         = situation.board
    val opponentColor = !situation.color
    lazy val fortress = hordeClosedPosition(situation) // costly function call
    if opponentColor == Color.white then
      lazy val notKingPieces           = InsufficientMatingMaterial.nonKingPieces(board) toList
      val horde                        = board.piecesOf(Color.white)
      lazy val hordeBishopSquareColors = horde.filter(_._2.is(Bishop)).toList.map(_._1.isLight).distinct
      lazy val hordeRoles              = horde.map(_._2.role)
      lazy val army                    = board.piecesOf(Color.black)
      lazy val armyPawnsOrRooks        = army.count(p => p._2.is(Pawn) || p._2.is(Rook))
      lazy val armyPawnsOrBishops      = army.filter(p => p._2.is(Pawn) || p._2.is(Bishop))
      lazy val armyPawnsOrKnights      = army.count(p => p._2.is(Pawn) || p._2.is(Knight))
      lazy val armyNonQueens           = army.count(_._2.isNot(Queen))
      lazy val armyNonQueensOrRooks    = army.count(p => p._2.isNot(Queen) && p._2.isNot(Rook))
      lazy val armyNonQueensOrBishops  = army.count(p => p._2.isNot(Queen) && p._2.isNot(Bishop))
      lazy val armyBishopSquareColors  = army.filter(_._2.is(Bishop)).toList.map(_._1.isLight).distinct
      if (horde.sizeIs == 1)
        hordeRoles match
          case List(Knight) =>
            army.sizeIs < 4 || armyNonQueensOrRooks == 0 || armyNonQueensOrBishops == 0 || (armyNonQueensOrBishops + armyBishopSquareColors.size) < 4
          case List(Bishop) =>
            notKingPieces.count(p =>
              p._2.is(Pawn) || (p._2.is(Bishop) && p._1.isLight != horde.head._1.isLight)
            ) < 2
          case List(Rook) => army.sizeIs < 3 || armyPawnsOrRooks == 0 || armyPawnsOrKnights == 0
          case _          => armyPawnsOrRooks == 0
      else if (
        (hordeRoles.forall(
          _ == Bishop
        ) && hordeBishopSquareColors.lengthCompare(1) == 0) && {
          armyPawnsOrKnights + armyPawnsOrBishops
            .count(p => p._1.isLight != horde.head._1.isLight) < 2
        }
      ) true
      else if (
        horde.sizeIs == 2 && hordeRoles
          .count(r => r == Queen || r == Rook || r == Pawn) < 2 && armyNonQueens <= 1
      )
        true
      else fortress
    else fortress

  extension (s: Situation)
    def hasBishopPair: Color => Boolean = side =>
      val bishops = s.board.bishops & s.board.byColor(side)
      bishops.intersects(Bitboard.lightSquares) && bishops.intersects(Bitboard.darkSquares)

    def byRole: Color => ByRole[Bitboard] = side =>
      ByRole[Bitboard](
        s.board.pawns & s.board.byColor(side),
        s.board.knights & s.board.byColor(side),
        s.board.bishops & s.board.byColor(side),
        s.board.rooks & s.board.byColor(side),
        s.board.queens & s.board.byColor(side),
        s.board.kings & s.board.byColor(side)
      )

  // port from Shakmaty
  import ByRole.*
  def hasInsufficientMaterial(situation: Situation): Boolean =
    import situation.{ board, color }
    import SquareColor.*
    // Black can always win by capturing the horde
    if !situation.isWhiteTurn then false
    else
      val horde                            = situation.byRole(Color.white)
      val hordeCount                       = horde.map(_.count)
      val hordeBishops: SquareColor => Int = color => (horde.bishop & color.bb).count
      val hordeBishopColor                 = if hordeBishops(Light) >= 1 then Light else Dark

      val hordeBishopNum = Math.min(hordeBishops(Light), 2) + Math.min(hordeBishops(Dark), 2)
      // Two same color bishops suffice to cover all the light and dark squares
      // around the enemy king.
      val hordeNum = hordeCount.pawn + hordeCount.knight + hordeCount.rook + hordeCount.queen + hordeBishopNum
      val pieces   = situation.byRole(Color.black)
      val piecesCount                       = pieces.map(_.count)
      val piecesBishops: SquareColor => Int = color => (pieces.bishop & color.bb).count
      val piecesNum                         = pieces.map(_.count).values.sum
      // horde or all?
      val piecesOfTypeNot = (pieces: Int) => piecesNum - pieces
      if hordeNum == 0 then true
      else if hordeNum >= 4 then false // Four or more white pieces can always deliver mate.
      // Pawns/queens are never insufficient material when paired with any other
      // piece (a pawn promotes to a queen and delivers mate).
      else if (hordeCount.pawn >= 1 || hordeCount.queen >= 1) && hordeNum >= 2 then false
      // A rook is insufficient material only when it is paired with a bishop
      // against a lone king. The horde can mate in any other case.
      // A rook on A1 and a bishop on C3 mate a king on B1 when there is a
      // friendly pawn/opposite-color-bishop/rook/queen on C2.
      // A rook on B8 and a bishop C3 mate a king on A1 when there is a friendly
      // knight on A2.
      else if (hordeCount.rook >= 1 && hordeNum >= 2) && !(hordeNum == 2 && hordeCount(Rook) == 1) then false
      else if hordeNum == 1 then
        if piecesNum == 1 then true // A lone piece cannot mate a lone King
        else if hordeCount.queen == 1 then
          // The horde has a lone queen.
          // A lone queen mates a king on A1 bounded by:
          //  -- a pawn/rook on A2
          //  -- two same color bishops on A2, B1
          // We ignore every other mating case, since it can be reduced to
          // the two previous cases (e.g. a black pawn on A2 and a black
          // bishop on B1).
          !(piecesCount.pawn >= 1 || piecesCount.rook >= 1 || piecesBishops(Light) >= 2 || piecesBishops(
            Dark
          ) >= 2)
        else if hordeCount.pawn == 1 then
          // Promote the pawn to a queen or a knight and check whether white
          // can mate.
          val pawnSquare =
            (situation.board.pawns & situation.board.byColor(
              Color.white
            )).first.get // we know there is a pawn
          val promoteToQueen = situation.board.putOrReplace(White.queen, pawnSquare).situationOf(Color.white)
          val promoteToKnight =
            situation.board.putOrReplace(White.knight, pawnSquare).situationOf(Color.white)
          hasInsufficientMaterial(promoteToQueen) && hasInsufficientMaterial(promoteToKnight)
        else if hordeCount.rook == 1 then
          // A lone rook mates a king on A8 bounded by a pawn/rook on A7 and a
          // pawn/knight on B7. We ignore every other case, since it can be
          // reduced to the two previous cases.
          // (e.g. three pawns on A7, B7, C7)
          !(piecesCount.pawn >= 2
            || (piecesCount.rook >= 1 && piecesCount.pawn >= 1)
            || (piecesCount.rook >= 1 && piecesCount.knight >= 1)
            || (piecesCount.pawn >= 1 && piecesCount.knight >= 1))
        else if hordeCount.bishop == 1 then // The horde has a lone bishop.
          // The king can be mated on A1 if there is a pawn/opposite-color-bishop
          // on A2 and an opposite-color-bishop on B1.
          // If black has two or more pawns, white gets the benefit of the doubt;
          // there is an outside chance that white promotes its pawns to
          // opposite-color-bishops and selfmates theirself.
          // Every other case that the king is mated by the bishop requires that
          // black has two pawns or two opposite-color-bishop or a pawn and an
          // opposite-color-bishop.
          // For example a king on A3 can be mated if there is
          // a pawn/opposite-color-bishop on A4, a pawn/opposite-color-bishop on
          // B3, a pawn/bishop/rook/queen on A2 and any other piece on B2.
          !(piecesBishops(!hordeBishopColor) >= 2
            || (piecesBishops(!hordeBishopColor) >= 1 && piecesCount.pawn >= 1)
            || piecesCount.pawn >= 2)
        else // the Horde has a lone knight
          // The king on A1 can be smother mated by a knight on C2 if there is
          // a pawn/knight/bishop on B2, a knight/rook on B1 and any other piece
          // on A2.
          // Moreover, when black has four or more pieces and two of them are
          // pawns, black can promote their pawns and selfmate theirself.
          !(piecesNum >= 4
            && (piecesCount.knight >= 2
              || piecesCount.pawn >= 2
              || (piecesCount.rook >= 1 && piecesCount.knight >= 1)
              || (piecesCount.rook >= 1 && piecesCount.bishop >= 1)
              || (piecesCount.rook >= 1 && piecesCount.pawn >= 1)
              || (piecesCount.knight >= 1 && piecesCount.bishop >= 1)
              || (piecesCount.knight >= 1 && piecesCount.pawn >= 1)
              || (piecesCount.bishop >= 1 && piecesCount.pawn >= 1)
              || (piecesCount.bishop >= 1 && piecesCount.pawn >= 1)
              || (situation.hasBishopPair(Black) && piecesCount.pawn >= 1))
            && (piecesBishops(Dark) < 2 || piecesOfTypeNot(piecesBishops(Dark)) >= 3)
            && (piecesBishops(Light) < 2 || piecesOfTypeNot(piecesBishops(Light)) >= 3))
      // By this point, we only need to deal with white's minor pieces.
      else if hordeNum == 2 then
        if piecesNum == 1 then true
        else if hordeCount.knight == 2 then piecesCount.pawn + piecesCount.bishop + piecesCount.knight < 1
        else if situation.hasBishopPair(White) then
          !(piecesCount.pawn >= 1 || piecesBishops(!hordeBishopColor) >= 1 ||
            piecesOfTypeNot(piecesBishops(hordeBishopColor)) >= 3)
        else
          // The horde has two or more bishops on the same color.
          // White can only win if black has enough material to obstruct
          // the squares of the opposite color around the king.
          // A king on A1 obstructed by a pawn/opposite-bishop/knight
          // on A2 and a opposite-bishop/knight on B1 is mated by two
          // bishops on B2 and C3. This position is theoretically
          // achievable even when black has two pawns or when they
          // have a pawn and an opposite color bishop.
          !((piecesCount.pawn >= 1 && piecesBishops(!hordeBishopColor) >= 1)
            || (piecesCount.pawn >= 1 && piecesCount.knight >= 1)
            || (piecesBishops(!hordeBishopColor) >= 1 && piecesCount.knight >= 1)
            || piecesBishops(!hordeBishopColor) >= 2
            || piecesCount.knight >= 2
            || piecesCount.pawn >= 2)
      else // hordeNum == 3
        if (hordeCount.knight == 2 && hordeCount.bishop == 1)
          || hordeCount.knight == 3
          || situation.hasBishopPair(White)
        then false
        else piecesNum == 1

enum SquareColor:
  case Light, Dark

  def bb = this match
    case Light => Bitboard.lightSquares
    case Dark  => Bitboard.darkSquares

  def unary_! = this match
    case Light => Dark
    case Dark  => Light

case class ByRole[A](pawn: A, knight: A, bishop: A, rook: A, queen: A, king: A):
  def apply(role: Role): A = role match
    case Pawn   => pawn
    case Knight => knight
    case Bishop => bishop
    case Rook   => rook
    case Queen  => queen
    case King   => king

  def values: List[A] = List(pawn, knight, bishop, rook, queen, king)

object ByRole:
  given Functor[ByRole] with
    def map[A, B](byRole: ByRole[A])(f: A => B): ByRole[B] =
      ByRole(
        f(byRole.pawn),
        f(byRole.knight),
        f(byRole.bishop),
        f(byRole.rook),
        f(byRole.queen),
        f(byRole.king)
      )
