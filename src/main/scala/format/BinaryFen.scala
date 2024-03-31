package chess
package format

import chess.bitboard.{ Bitboard, Board as BBoard }
import chess.variant.*

import scala.collection.mutable.ArrayBuilder

opaque type BinaryFen = Array[Byte]

object BinaryFen:
  extension (bf: BinaryFen)
    def value: Array[Byte] = bf

    def read: Situation.AndFullMoveNumber =
      val reader = new Iterator[Byte]:
        val inner                            = bf.iterator
        override inline def hasNext: Boolean = inner.hasNext
        override inline def next: Byte       = if hasNext then inner.next else 0.toByte

      val occupied = Bitboard(readLong(reader))

      var pawns   = Bitboard.empty
      var knights = Bitboard.empty
      var bishops = Bitboard.empty
      var rooks   = Bitboard.empty
      var queens  = Bitboard.empty
      var kings   = Bitboard.empty
      var white   = Bitboard.empty
      var black   = Bitboard.empty

      var turn                     = White
      var unmovedRooks             = UnmovedRooks(Bitboard.empty)
      var epMove: Option[Uci.Move] = None

      def unpackPiece(sq: Square, nibble: Int) =
        val bb = sq.bb
        nibble match
          case 0 =>
            pawns |= bb
            white |= bb
          case 1 =>
            pawns |= bb
            black |= bb
          case 2 =>
            knights |= bb
            white |= bb
          case 3 =>
            knights |= bb
            black |= bb
          case 4 =>
            bishops |= bb
            white |= bb
          case 5 =>
            bishops |= bb
            black |= bb
          case 6 =>
            rooks |= bb
            white |= bb
          case 7 =>
            rooks |= bb
            black |= bb
          case 8 =>
            queens |= bb
            white |= bb
          case 9 =>
            queens |= bb
            black |= bb
          case 10 =>
            kings |= bb
            white |= bb
          case 11 =>
            kings |= bb
            black |= bb
          case 12 =>
            pawns |= bb
            epMove = Some(Uci.Move(sq.xor(Square.A3), sq))
            if sq.rank.value < 4 then white |= bb
            else black |= bb
          case 13 =>
            rooks |= bb
            white |= bb
            unmovedRooks |= bb
          case 14 =>
            rooks |= bb
            black |= bb
            unmovedRooks |= bb
          case 15 =>
            kings |= bb
            black |= bb
            turn = Black
          case _ =>

      val it = occupied.iterator
      while it.hasNext
      do
        val (lo, hi) = readNibbles(reader)
        unpackPiece(it.next, lo)
        if it.hasNext then unpackPiece(it.next, hi)

      val halfMoveClock = HalfMoveClock(readLeb128(reader))
      var ply           = Ply(readLeb128(reader))
      val variant = reader.next match
        case 1 => ThreeCheck
        case 2 => Antichess
        case 3 => Atomic
        case 4 => Horde
        case 5 => RacingKings
        case 6 => Crazyhouse
        case 7 => KingOfTheHill
        case _ => Standard

      if ply.turn.black then turn = Black

      val checkCount = if variant.threeCheck then
        val (lo, hi) = readNibbles(reader)
        CheckCount(white = lo, black = hi)
      else CheckCount()

      val crazyData = if variant.crazyhouse then
        val (wp, bp) = readNibbles(reader)
        val (wn, bn) = readNibbles(reader)
        val (wb, bb) = readNibbles(reader)
        val (wr, br) = readNibbles(reader)
        val (wq, bq) = readNibbles(reader)
        Some(
          Crazyhouse.Data(
            pockets = ByColor(
              white = Crazyhouse.Pocket(pawn = wp, knight = wn, bishop = wb, rook = wr, queen = wq),
              black = Crazyhouse.Pocket(pawn = bp, knight = bn, bishop = bb, rook = br, queen = bq)
            ),
            promoted = Bitboard(readLong(reader))
          )
        )
      else None

      Situation.AndFullMoveNumber(
        Situation(
          Board(
            BBoard(
              occupied = occupied,
              white = white,
              black = black,
              pawns = pawns,
              knights = knights,
              bishops = bishops,
              rooks = rooks,
              queens = queens,
              kings = kings
            ),
            History(
              lastMove = epMove,
              checkCount = checkCount,
              castles =
                maximumCastles(unmovedRooks = unmovedRooks, white = white, black = black, kings = kings),
              unmovedRooks = unmovedRooks,
              halfMoveClock = halfMoveClock
            ),
            variant,
            crazyData
          ),
          color = turn
        ),
        ply.fullMoveNumber
      )

  def write(input: Situation.AndFullMoveNumber): BinaryFen =
    val builder = ArrayBuilder.ofByte()

    val sit      = input.situation
    val occupied = sit.board.occupied
    writeLong(builder, occupied.value)

    val unmovedRooks                 = minimumUnmovedRooks(sit.board)
    val pawnPushedTo: Option[Square] = sit.enPassantSquare.map(_.xor(Square.A2))

    def packPiece(sq: Square): Byte =
      sit.board(sq) match
        // Encoding from
        // https://github.com/official-stockfish/nnue-pytorch/blob/2db3787d2e36f7142ea4d0e307b502dda4095cd9/lib/nnue_training_data_formats.h#L4607
        case Some(Piece(_, Pawn)) if pawnPushedTo.contains(sq) => 12
        case Some(Piece(White, Pawn))                          => 0
        case Some(Piece(Black, Pawn))                          => 1
        case Some(Piece(White, Knight))                        => 2
        case Some(Piece(Black, Knight))                        => 3
        case Some(Piece(White, Bishop))                        => 4
        case Some(Piece(Black, Bishop))                        => 5
        case Some(Piece(White, Rook))                          => if unmovedRooks.contains(sq) then 13 else 6
        case Some(Piece(Black, Rook))                          => if unmovedRooks.contains(sq) then 14 else 7
        case Some(Piece(White, Queen))                         => 8
        case Some(Piece(Black, Queen))                         => 9
        case Some(Piece(White, King))                          => 10
        case Some(Piece(Black, King))                          => if sit.color.white then 11 else 15
        case None                                              => 0 // unreachable

    val it = occupied.iterator
    while it.hasNext
    do writeNibbles(builder, packPiece(it.next), if it.hasNext then packPiece(it.next) else 0)

    val halfMoveClock = sit.history.halfMoveClock.value
    val ply           = input.fullMoveNumber.ply(sit.color).value
    val brokenTurn    = sit.color.black && sit.board(Black, King).isEmpty
    val variantHeader = sit.variant match
      case Standard | Chess960 | FromPosition => 0
      case ThreeCheck                         => 1
      case Antichess                          => 2
      case Atomic                             => 3
      case Horde                              => 4
      case RacingKings                        => 5
      case Crazyhouse                         => 6
      case KingOfTheHill                      => 7

    if halfMoveClock > 0 || ply > 1 || brokenTurn || variantHeader != 0
    then writeLeb128(builder, sit.history.halfMoveClock.value)

    if ply > 1 || brokenTurn || variantHeader != 0
    then writeLeb128(builder, ply)

    if variantHeader != 0
    then
      builder.addOne(variantHeader.toByte)
      if sit.variant.threeCheck then
        writeNibbles(builder, sit.history.checkCount.white, sit.history.checkCount.black)
      else if sit.variant.crazyhouse then
        val crazyData = sit.board.crazyData.getOrElse(Crazyhouse.Data.init)
        val pockets   = crazyData.pockets
        writeNibbles(builder, pockets.white.pawn, pockets.black.pawn)
        writeNibbles(builder, pockets.white.knight, pockets.black.knight)
        writeNibbles(builder, pockets.white.bishop, pockets.black.bishop)
        writeNibbles(builder, pockets.white.rook, pockets.black.rook)
        writeNibbles(builder, pockets.white.queen, pockets.black.queen)
        writeLong(builder, crazyData.promoted.value)

    builder.result

  private def writeLong(builder: ArrayBuilder[Byte], v: Long) =
    builder.addOne((v >>> 56).toByte)
    builder.addOne((v >>> 48).toByte)
    builder.addOne((v >>> 40).toByte)
    builder.addOne((v >>> 32).toByte)
    builder.addOne((v >>> 24).toByte)
    builder.addOne((v >>> 16).toByte)
    builder.addOne((v >>> 8).toByte)
    builder.addOne(v.toByte)

  private def readLong(reader: Iterator[Byte]): Long =
    ((reader.next & 0xffL) << 56) |
      ((reader.next & 0xffL) << 48) |
      ((reader.next & 0xffL) << 40) |
      ((reader.next & 0xffL) << 32) |
      ((reader.next & 0xffL) << 24) |
      ((reader.next & 0xffL) << 16) |
      ((reader.next & 0xffL) << 8) |
      (reader.next & 0xffL)

  private def writeLeb128(builder: ArrayBuilder[Byte], v: Int) =
    var n = v
    while n > 127
    do
      builder.addOne((n | 128).toByte)
      n = n >>> 7
    builder.addOne(n.toByte)

  private def readLeb128(reader: Iterator[Byte]): Int =
    var n     = 0
    var shift = 0
    while
      val b = reader.next
      n |= (b & 127) << shift
      shift += 7
      (b & 128) != 0
    do ()
    n

  private def writeNibbles(builder: ArrayBuilder[Byte], lo: Int, hi: Int) =
    builder.addOne((lo | (hi << 4)).toByte)

  private def readNibbles(reader: Iterator[Byte]): (Int, Int) =
    val b = reader.next
    ((b & 0xf), (b >>> 4) & 0xf)

  private def minimumUnmovedRooks(board: Board): UnmovedRooks =
    val white   = board.history.unmovedRooks.bb & board.white & Bitboard.firstRank
    val black   = board.history.unmovedRooks.bb & board.black & Bitboard.lastRank
    val castles = board.history.castles
    UnmovedRooks(
      (if castles.whiteKingSide then white.isolateLast else Bitboard.empty) |
        (if castles.whiteQueenSide then white.isolateFirst else Bitboard.empty) |
        (if castles.blackKingSide then black.isolateLast else Bitboard.empty) |
        (if castles.blackQueenSide then black.isolateFirst else Bitboard.empty)
    )

  private def maximumCastles(
      unmovedRooks: UnmovedRooks,
      white: Bitboard,
      black: Bitboard,
      kings: Bitboard
  ): Castles =
    val whiteRooks = unmovedRooks.bb & white & Bitboard.firstRank
    val blackRooks = unmovedRooks.bb & black & Bitboard.lastRank
    val whiteKing  = (white & kings & Bitboard.firstRank).first.getOrElse(Square.E1)
    val blackKing  = (black & kings & Bitboard.lastRank).first.getOrElse(Square.E8)
    Castles(
      whiteKingSide = whiteRooks.last.exists(r => r.value > whiteKing.value),
      whiteQueenSide = whiteRooks.first.exists(r => r.value < whiteKing.value),
      blackKingSide = blackRooks.last.exists(r => r.value > blackKing.value),
      blackQueenSide = blackRooks.first.exists(r => r.value < blackKing.value)
    )
