package chess
package format

import chess.variant.*

import scala.collection.mutable.ArrayBuilder

case class BinaryFen(value: Array[Byte]) extends AnyVal:

  import BinaryFen.implementation.*

  def read: Position.AndFullMoveNumber =
    val reader = new Iterator[Byte]:
      val inner                            = value.iterator
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
          epMove = Some(Uci.Move(Square.unsafe(sq.value ^ 0x10), sq))
          if sq.rank <= Rank.Fourth then white |= bb
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
    val ply           = Ply(readLeb128(reader))
    val variant = reader.next match
      case 0 => Standard
      case 1 => Crazyhouse
      case 2 => Chess960
      case 3 => FromPosition
      case 4 => KingOfTheHill
      case 5 => ThreeCheck
      case 6 => Antichess
      case 7 => Atomic
      case 8 => Horde
      case 9 => RacingKings
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

    Position.AndFullMoveNumber(
      Position(
        Board(
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
          castles = maximumCastles(unmovedRooks = unmovedRooks, white = white, black = black, kings = kings),
          unmovedRooks = unmovedRooks,
          halfMoveClock = halfMoveClock,
          crazyData = crazyData
        ),
        variant,
        turn
      ),
      ply.fullMoveNumber
    )

  override def hashCode: Int = value.toSeq.hashCode
  override def equals(that: Any): Boolean = that match
    case thatFen: BinaryFen => value.toSeq.equals(thatFen.value.toSeq)
    case _                  => false

object BinaryFen:

  import BinaryFen.implementation.*

  def writeNormalized(board: Position): BinaryFen =
    write(
      Position.AndFullMoveNumber(
        board
          .updateHistory(_.setHalfMoveClock(HalfMoveClock.initial))
          .withVariant(board.variant match
            case Standard | Chess960 | FromPosition => Standard
            case other                              => other),
        FullMoveNumber.initial
      )
    )

  def write(input: Position.AndFullMoveNumber) = BinaryFen:
    val builder = ArrayBuilder.ofByte()
    builder.sizeHint(8 + 32)

    val sit      = input.board
    val occupied = sit.board.occupied
    writeLong(builder, occupied.value)

    val unmovedRooks = minimumUnmovedRooks(sit)
    val pawnPushedTo = sit.enPassantSquare.flatMap(_.prevRank(sit.color))

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
      case Standard      => 0
      case Crazyhouse    => 1
      case Chess960      => 2
      case FromPosition  => 3
      case KingOfTheHill => 4
      case ThreeCheck    => 5
      case Antichess     => 6
      case Atomic        => 7
      case Horde         => 8
      case RacingKings   => 9

    if halfMoveClock > 0 || ply > 1 || brokenTurn || variantHeader != 0
    then writeLeb128(builder, halfMoveClock)

    if ply > 1 || brokenTurn || variantHeader != 0
    then writeLeb128(builder, ply)

    if variantHeader != 0
    then
      builder.addOne(variantHeader.toByte)
      if sit.variant.threeCheck then
        writeNibbles(builder, sit.history.checkCount.white, sit.history.checkCount.black)
      else if sit.variant.crazyhouse then
        val crazyData = sit.crazyData.getOrElse(Crazyhouse.Data.init)
        val pockets   = crazyData.pockets
        writeNibbles(builder, pockets.white.pawn, pockets.black.pawn)
        writeNibbles(builder, pockets.white.knight, pockets.black.knight)
        writeNibbles(builder, pockets.white.bishop, pockets.black.bishop)
        writeNibbles(builder, pockets.white.rook, pockets.black.rook)
        writeNibbles(builder, pockets.white.queen, pockets.black.queen)
        if crazyData.promoted.nonEmpty then writeLong(builder, crazyData.promoted.value)

    builder.result

  object implementation:

    def writeLong(builder: ArrayBuilder[Byte], v: Long) =
      builder.addOne((v >>> 56).toByte)
      builder.addOne((v >>> 48).toByte)
      builder.addOne((v >>> 40).toByte)
      builder.addOne((v >>> 32).toByte)
      builder.addOne((v >>> 24).toByte)
      builder.addOne((v >>> 16).toByte)
      builder.addOne((v >>> 8).toByte)
      builder.addOne(v.toByte)

    def readLong(reader: Iterator[Byte]): Long =
      ((reader.next & 0xffL) << 56) |
        ((reader.next & 0xffL) << 48) |
        ((reader.next & 0xffL) << 40) |
        ((reader.next & 0xffL) << 32) |
        ((reader.next & 0xffL) << 24) |
        ((reader.next & 0xffL) << 16) |
        ((reader.next & 0xffL) << 8) |
        (reader.next & 0xffL)

    def writeLeb128(builder: ArrayBuilder[Byte], v: Int) =
      var n = v
      while n > 127
      do
        builder.addOne((n | 128).toByte)
        n = n >>> 7
      builder.addOne(n.toByte)

    def readLeb128(reader: Iterator[Byte]): Int =
      var n     = 0
      var shift = 0
      while
        val b = reader.next
        n |= (b & 127) << shift
        shift += 7
        (b & 128) != 0
      do ()
      n & 0x7fff_ffff

    def writeNibbles(builder: ArrayBuilder[Byte], lo: Int, hi: Int) =
      builder.addOne((lo | (hi << 4)).toByte)

    def readNibbles(reader: Iterator[Byte]): (Int, Int) =
      val b = reader.next
      ((b & 0xf), (b >>> 4) & 0xf)

    def minimumUnmovedRooks(board: Position): UnmovedRooks =
      val white   = board.history.unmovedRooks.bb & board.white & Bitboard.firstRank
      val black   = board.history.unmovedRooks.bb & board.black & Bitboard.lastRank
      val castles = board.history.castles
      UnmovedRooks(
        (if castles.whiteKingSide then white.isolateLast else Bitboard.empty) |
          (if castles.whiteQueenSide then white.isolateFirst else Bitboard.empty) |
          (if castles.blackKingSide then black.isolateLast else Bitboard.empty) |
          (if castles.blackQueenSide then black.isolateFirst else Bitboard.empty)
      )

    def maximumCastles(
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
