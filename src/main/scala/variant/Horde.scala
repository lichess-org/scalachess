package chess
package variant

import chess.format.EpdFen
import chess.bitboard.Bitboard
import chess.bitboard.Bitboard.*
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
  lazy val pieces: Map[Square, Piece] =
    val whitePawnsHorde = for
      x <- File.all
      y <- Rank.all.take(4)
    yield (Square(x, y) -> White.pawn)
    val frontPawns  = List(Square.B5, Square.C5, Square.F5, Square.G5).map { _ -> White.pawn }
    val blackPawns  = File.all.map { Square(_, Rank.Seventh) -> Black.pawn }
    val blackPieces = File.all.map { x => Square(x, Rank.Eighth) -> (Black - backRank(x.index)) }
    whitePawnsHorde ++ frontPawns ++ blackPawns ++ blackPieces toMap

  override val castles = Castles("kq")

  override val initialFen = EpdFen(
    "rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1"
  )

  def validMoves(situation: Situation): List[Move] =
    import situation.{ genEnPassant, genNonKing, isWhiteTurn, us, board }
    if isWhiteTurn then genEnPassant(us & board.pawns) ++ genNonKing(~us & ~board.kings)
    else Standard.validMoves(situation)

  override def valid(situation: Situation, strict: Boolean) =
    situation.board.kingOf(White).isEmpty && validSide(situation.board, strict)(
      Black
    ) && !pawnsOnPromotionRank(situation.board, White)

  /** The game has a special end condition when black manages to capture all of white's pawns */
  override def specialEnd(situation: Situation) =
    situation.board.white.isEmpty

  /** Any vs K + any where horde is stalemated and only king can move is a fortress draw
    * This does not consider imminent fortresses such as 8/p7/P7/8/8/P7/8/k7 b - -
    * nor does it consider contrived fortresses such as b7/pk6/P7/P7/8/8/8/8 b - -
    */
  private def hordeClosedPosition(situation: Situation): Boolean =
    val hordeSquare = situation.board.byColor(White)
    val mateInOne = hordeSquare.count == 1 &&
      hordeSquare.singleSquare.exists(pieceThreatened(situation.board, Color.black, _))
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
    hasInsufficientMaterial(situation.board, !situation.color) || hordeClosedPosition(situation)

  extension (board: Board)
    def hasBishopPair: Color => Boolean = side =>
      val bishops = board.bishops & board.byColor(side)
      bishops.intersects(Bitboard.lightSquares) && bishops.intersects(Bitboard.darkSquares)

  def hasInsufficientMaterial(board: Board, color: Color): Boolean =
    import SquareColor.*
    import Bitboard.*
    // Black can always win by capturing the horde
    if color.black then false
    else
      val hordeRole                        = board.byRoleOf(color)
      val horde                            = hordeRole.map(_.count)
      val hordeBishops: SquareColor => Int = color => (hordeRole.bishop & color.bb).count
      val hordeBishopColor                 = if hordeBishops(Light) >= 1 then Light else Dark

      val hordeBishopNum = Math.min(hordeBishops(Light), 2) + Math.min(hordeBishops(Dark), 2)
      // Two same color bishops suffice to cover all the light and dark squares
      // around the enemy king.
      val hordeNum   = horde.pawn + horde.knight + horde.rook + horde.queen + hordeBishopNum
      val piecesRole = board.byRoleOf(Color.black)
      val pieces     = piecesRole.map(_.count)
      val piecesBishops: SquareColor => Int = color => (piecesRole.bishop & color.bb).count
      val piecesNum                         = piecesRole.map(_.count).values.sum
      val piecesOfTypeNot                   = (pieces: Int) => piecesNum - pieces
      if hordeNum == 0 then true
      else if hordeNum >= 4 then false // Four or more white pieces can always deliver mate.
      // Pawns/queens are never insufficient material when paired with any other
      // piece (a pawn promotes to a queen and delivers mate).
      else if (horde.pawn >= 1 || horde.queen >= 1) && hordeNum >= 2 then false
      // A rook is insufficient material only when it is paired with a bishop
      // against a lone king. The horde can mate in any other case.
      // A rook on A1 and a bishop on C3 mate a king on B1 when there is a
      // friendly pawn/opposite-color-bishop/rook/queen on C2.
      // A rook on B8 and a bishop C3 mate a king on A1 when there is a friendly
      // knight on A2.
      else if (horde.rook >= 1 && hordeNum >= 2)
        && !(hordeNum == 2 && horde(Rook) == 1
          && horde.bishop == 1
          && piecesOfTypeNot(piecesBishops(hordeBishopColor)) == 1)
      then false
      else if hordeNum == 1 then
        if piecesNum == 1 then true // A lone piece cannot mate a lone King
        else if horde.queen == 1 then
          // The horde has a lone queen.
          // A lone queen mates a king on A1 bounded by:
          //  -- a pawn/rook on A2
          //  -- two same color bishops on A2, B1
          // We ignore every other mating case, since it can be reduced to
          // the two previous cases (e.g. a black pawn on A2 and a black
          // bishop on B1).
          !(pieces.pawn >= 1 || pieces.rook >= 1 || piecesBishops(Light) >= 2 || piecesBishops(
            Dark
          ) >= 2)
        else if horde.pawn == 1 then
          // Promote the pawn to a queen or a knight and check whether white
          // can mate.
          val pawnSquare     = (board.pawns & board.byColor(Color.white)).first.get // we know there is a pawn
          val promoteToQueen = board.putOrReplace(White.queen, pawnSquare)
          val promoteToKnight = board.putOrReplace(White.knight, pawnSquare)
          hasInsufficientMaterial(promoteToQueen, color) && hasInsufficientMaterial(promoteToKnight, color)
        else if horde.rook == 1 then
          // A lone rook mates a king on A8 bounded by a pawn/rook on A7 and a
          // pawn/knight on B7. We ignore every other case, since it can be
          // reduced to the two previous cases.
          // (e.g. three pawns on A7, B7, C7)
          !(pieces.pawn >= 2
            || (pieces.rook >= 1 && pieces.pawn >= 1)
            || (pieces.rook >= 1 && pieces.knight >= 1)
            || (pieces.pawn >= 1 && pieces.knight >= 1))
        else if horde.bishop == 1 then // The horde has a lone bishop.
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
            || (piecesBishops(!hordeBishopColor) >= 1 && pieces.pawn >= 1)
            || pieces.pawn >= 2)
        else // the Horde has a lone knight
          // The king on A1 can be smother mated by a knight on C2 if there is
          // a pawn/knight/bishop on B2, a knight/rook on B1 and any other piece
          // on A2.
          // Moreover, when black has four or more pieces and two of them are
          // pawns, black can promote their pawns and selfmate theirself.
          !(piecesNum >= 4
            && (pieces.knight >= 2
              || pieces.pawn >= 2
              || (pieces.rook >= 1 && pieces.knight >= 1)
              || (pieces.rook >= 1 && pieces.bishop >= 1)
              || (pieces.rook >= 1 && pieces.pawn >= 1)
              || (pieces.knight >= 1 && pieces.bishop >= 1)
              || (pieces.knight >= 1 && pieces.pawn >= 1)
              || (pieces.bishop >= 1 && pieces.pawn >= 1)
              || (pieces.bishop >= 1 && pieces.pawn >= 1)
              || (board.hasBishopPair(Black) && pieces.pawn >= 1))
            && (piecesBishops(Dark) < 2 || piecesOfTypeNot(piecesBishops(Dark)) >= 3)
            && (piecesBishops(Light) < 2 || piecesOfTypeNot(piecesBishops(Light)) >= 3))
      // By this point, we only need to deal with white's minor pieces.
      else if hordeNum == 2 then
        if piecesNum == 1 then true
        else if horde.knight == 2 then
          // A king on A1 is mated by two knights, if it is obstructed by a
          // pawn/bishop/knight on B2. On the other hand, if black only has
          // major pieces it is a draw.
          pieces.pawn + pieces.bishop + pieces.knight < 1
        else if board.hasBishopPair(color) then
          // A king on A1 obstructed by a pawn/bishop on A2 is mated
          // by the bishop pair.
          !(pieces.pawn >= 1 || pieces.bishop >= 1 ||
            // A pawn/bishop/knight on B4, a pawn/bishop/rook/queen on
            // A4 and the king on A3 enable Boden's mate by the bishop
            // pair. In every other case white cannot win.
            (pieces.knight >= 1 && pieces.rook + pieces.queen >= 1))
        else if horde.bishop >= 1 && horde.knight >= 1 then
          // The horde has a bishop and a knight.
          // A king on A1 obstructed by a pawn/opposite-color-bishop on
          // A2 is mated by a knight on D2 and a bishop on C3.
          !(pieces.pawn >= 1 || piecesBishops(!hordeBishopColor) >= 1 ||
            // A king on A1 bounded by two friendly pieces on A2 and B1 is
            // mated when the knight moves from D4 to C2 so that both the
            // knight and the bishop deliver check.
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
          !((pieces.pawn >= 1 && piecesBishops(!hordeBishopColor) >= 1)
            || (pieces.pawn >= 1 && pieces.knight >= 1)
            || (piecesBishops(!hordeBishopColor) >= 1 && pieces.knight >= 1)
            || piecesBishops(!hordeBishopColor) >= 2
            || pieces.knight >= 2
            || pieces.pawn >= 2)
      // hordeNum == 3
      else
      // A king in the corner is mated by two knights and a bishop or three
      // knights or the bishop pair and a knight/bishop.
      if (horde.knight == 2 && horde.bishop == 1)
        || horde.knight == 3
        || board.hasBishopPair(White)
      then false
      // White has two same color bishops and a knight.
      // A king on A1 is mated by a bishop on B2, a bishop on C1 and a
      // knight on C3, as long as there is another black piece to waste
      // a tempo.
      else piecesNum == 1

enum SquareColor:
  case Light
  case Dark

  def bb = this match
    case Light => Bitboard.lightSquares
    case Dark  => Bitboard.darkSquares

  def unary_! = this match
    case Light => Dark
    case Dark  => Light
