package chess
package bitboard

import Bitboard.*
import scala.collection.mutable.ListBuffer
import cats.syntax.all.*
import chess.variant.Variant
import chess.variant.Crazyhouse

// similar to chess.Situation
case class Fen(board: Board, state: State):
  def us: Bitboard               = board.byColor(state.turn)
  def them: Bitboard             = board.byColor(!state.turn)
  def ourKing: Option[Pos]       = board.king(state.turn)
  def checkers: Option[Bitboard] = ourKing.map(board.attacksTo(_, !state.turn))
  def sliderBlockers: Bitboard   = board.sliderBlockers(state.turn)
  def isWhiteTurn: Boolean       = state.turn.white
  def occupied: Bitboard         = board.occupied
  def isOccupied: Pos => Boolean = board.isOccupied

  // Used for filtering candidate moves that would leave put the king in check.
  def isSafe(king: Pos, move: Move, blockers: Bitboard): Boolean =
    move match
      case Move.Normal(from, to, _, _) =>
        val result = !(us & blockers).contains(from.value) || Bitboard.aligned(from, to, king)
        result
      case Move.EnPassant(from, to) =>
        val newOccupied = (occupied ^ from.bitboard ^ to.withRankOf(from).bitboard) | to.bitboard
        (king.rookAttacks(newOccupied) & them & (board.rooks ^ board.queens)).isEmpty &&
        (king.bishopAttacks(newOccupied) & them & (board.bishops ^ board.queens)).isEmpty
      case _ => true

  // TODO now it works with valid move only
  def play(move: Move): Fen =
    Fen(playBoard(move), playState(move))

  // TODO should we validate the move here?
  // Either[InvalidMove, Board]
  def playBoard: Move => Board =
    board.play(state.turn)

  def playState(move: Move): State =
    val halfMoves = if move.isHalfMove then state.halfMoves + 1 else HalfMoveClock(0)
    val fullMoves = if state.turn.black then state.fullMoves + 1 else state.fullMoves
    val turn      = !state.turn
    val halfCastlingRights =
      if move.isCapture then state.castlingRights & ~move.to.bitboard
      else state.castlingRights
    val haftState = state.copy(
      turn = turn,
      halfMoves = halfMoves,
      fullMoves = fullMoves,
      epSquare = None,
      castlingRights = halfCastlingRights
    )

    move match
      case Move.Normal(from, to, Pawn, _) =>
        val epSquare: Option[Pos] =
          if Math.abs((from - to).value) == 16 then
            // TODO calculate their pawns attacks
            Some(Pos(from.value + (if isWhiteTurn then 8 else -8)))
          else None
        haftState.copy(epSquare = epSquare)
      case Move.Normal(from, _, Rook, _) =>
        val castlingRights = halfCastlingRights & ~from.bitboard
        haftState.copy(castlingRights = castlingRights)
      case Move.Normal(_, _, King, _) | Move.Castle(_, _) =>
        val castlingRights = halfCastlingRights & Bitboard.rank(state.turn.lastRank)
        haftState.copy(castlingRights = castlingRights)
      case _ => haftState

enum ParseFenError:
  case InvalidFenFormat
  case InvalidBoard
  case InvalidColor
  case InvalidTurn
  case InvalidEpSquare
  case InvalidCastling
  case InvalidHalfMoveClock
  case InvalidFullMoveClock

/** Fen parser temporary put it here for testing purpose
  */
object Fen:

  val standard = Fen(Board.standard, State.start)

  def parse(fen: String): Either[ParseFenError, Fen] =
    val parts = fen.split(' ').filter(s => !s.isEmpty)
    if parts.size != 6 then Left(ParseFenError.InvalidFenFormat)
    else
      for
        board          <- parseBoard(parts(0))
        turn           <- parseColor(parts(1))
        castlingRights <- parseCastlingRights(parts(2))
        epSquare       <- parseEpPassantSquare(parts(3))
        halfMoves      <- HalfMoveClock from parts(4).toIntOption.toRight(ParseFenError.InvalidHalfMoveClock)
        fullMoves      <- FullMoveNumber from parts(5).toIntOption.toRight(ParseFenError.InvalidHalfMoveClock)
      yield Fen(board, State(turn, epSquare, castlingRights, halfMoves, fullMoves))

  def parseColor(s: String): Either[ParseFenError, Color] =
    s match
      case "w" => Right(Color.White)
      case "b" => Right(Color.Black)
      case _   => Left(ParseFenError.InvalidColor)

  def parseCastlingRights(s: String): Either[ParseFenError, Bitboard] =
    s match
      case "-" => Right(Bitboard.empty)
      case _ =>
        s.toList
          .traverse(charToSquare)
          .map(_.foldRight(Bitboard.empty)((s, b) => s.bitboard | b))
          .toRight(ParseFenError.InvalidCastling)

  // TODO naming is hard
  def fromString(s: String): Option[Pos] =
    s.toList match
      case x :: y :: Nil => Pos.at(x, y)
      case _             => None

  def parseEpPassantSquare(s: String): Either[ParseFenError, Option[Pos]] =
    val epSquares = for
      f <- 'a' to 'h'
      r <- List('3', '6')
    yield s"$f$r"
    s match
      case ep if epSquares contains ep => Right(fromString(ep))
      case "-"                         => Right(None)
      case _                           => Left(ParseFenError.InvalidEpSquare)

  def charToSquare: (c: Char) => Option[Pos] =
    case 'k' => Some(Pos(File.H, Rank.Eighth))
    case 'q' => Some(Pos(File.A, Rank.Eighth))
    case 'K' => Some(Pos(File.H, Rank.First))
    case 'Q' => Some(Pos(File.A, Rank.First))
    case _   => None

  def parseBoard(boardFen: String): Either[ParseFenError, Board] =
    var rank   = 7
    var file   = 0
    val iter   = boardFen.iterator
    val pieces = ListBuffer[(Pos, Piece)]()
    while iter.hasNext
    do
      iter.next match
        case '/' if file == 8 => {
          file = 0
          rank -= 1
          if rank < 0 then return Left(ParseFenError.InvalidBoard)
        }
        case ch if '1' to '8' contains ch => {
          file += (ch - '0')
          if file > 8 then return Left(ParseFenError.InvalidBoard)
        }
        case ch => {
          // println(s"do $ch $file $rank")
          (Piece.fromChar(ch), File.atIndex(file), Rank.atIndex(rank))
            .mapN((p, f, r) => (Pos(f, r), p))
            .match
              case Some(p) => pieces.addOne(p)
              case None    => return Left(ParseFenError.InvalidBoard)
          file += 1
        }
    Right(Board.fromMap(pieces.toMap))
