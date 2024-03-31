package chess
package format

import scala.collection.mutable.ArrayBuilder
import chess.variant.*
import chess.bitboard.{ Bitboard, Board as BBoard }

opaque type BinaryFen = Array[Byte]

object BinaryFen:
  extension (bf: BinaryFen)
    def read: Situation.AndFullMoveNumber =
      val reader = new Iterator[Byte]:
        val inner                   = bf.iterator
        inline def hasNext: Boolean = inner.hasNext
        inline def next: Byte       = if hasNext then inner.next else 0.toByte

      val occupied     = Bitboard(readLong(reader))
      var pawns        = Bitboard.empty
      var knights      = Bitboard.empty
      var bishops      = Bitboard.empty
      var rooks        = Bitboard.empty
      var queens       = Bitboard.empty
      var kings        = Bitboard.empty
      var white        = Bitboard.empty
      var black        = Bitboard.empty
      var unmovedRooks = Bitboard.empty
      var turn         = White

      def unpackPiece(sq: Square, nibble: Int) = ???

      val it = occupied.iterator
      while it.hasNext
      do
        val (lo, hi) = readNibbles(reader)
        unpackPiece(it.next, lo)
        if it.hasNext then unpackPiece(it.next, hi)

      val halfMoves     = readLeb128(reader)
      var ply           = Ply(readLeb128(reader))
      val variantHeader = reader.next

      if ply.turn.black then turn = Black

      val variant = variantHeader match
        case 1 => ThreeCheck
        case 2 => Antichess
        case 3 => Atomic
        case 4 => Horde
        case 5 => RacingKings
        case 6 => Crazyhouse
        case _ => Standard

      val checkCount = if variant.threeCheck then
        val (lo, hi) = readNibbles(reader)
        CheckCount(lo, hi)
      else CheckCount()

      val crazyData = if variant.crazyhouse then
        val promoted = readLong(reader)
        val (wp, bp) = readNibbles(reader)
        val (wn, bn) = readNibbles(reader)
        val (wb, bb) = readNibbles(reader)
        val (wr, br) = readNibbles(reader)
        val (wq, bq) = readNibbles(reader)
        Some(
          Crazyhouse.Data(
            ByColor(
              white = Crazyhouse.Pocket(pawn = wp, knight = wn, bishop = wb, rook = wr, queen = wq),
              black = Crazyhouse.Pocket(pawn = bp, knight = bn, bishop = bb, rook = br, queen = bq)
            ),
            Bitboard(promoted)
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
            History(unmovedRooks = UnmovedRooks(unmovedRooks)),
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
    addLong(builder, occupied.value)

    val pawnPushedTo: Option[Square] = sit.enPassantSquare.map(_.xor(Square.A2))

    def packPiece(sq: Square): Byte =
      sit.board(sq) match
        case Some(Piece(_, Pawn)) if pawnPushedTo.contains(sq) => 12
        case Some(Piece(White, Pawn))                          => 0
        case Some(Piece(Black, Pawn))                          => 1
        case Some(Piece(White, Knight))                        => 2
        case Some(Piece(Black, Knight))                        => 3
        case Some(Piece(White, Bishop))                        => 4
        case Some(Piece(Black, Bishop))                        => 5
        case Some(Piece(White, Rook))  => if sit.history.unmovedRooks.contains(sq) then 13 else 6
        case Some(Piece(Black, Rook))  => if sit.history.unmovedRooks.contains(sq) then 14 else 7
        case Some(Piece(White, Queen)) => 8
        case Some(Piece(Black, Queen)) => 9
        case Some(Piece(White, King))  => 10
        case Some(Piece(Black, King))  => if sit.color.white then 11 else 15
        case None                      => 0 // unreachable

    val it = occupied.iterator
    while it.hasNext
    do addNibbles(builder, packPiece(it.next), if it.hasNext then packPiece(it.next) else 0)

    val halfMoves  = sit.history.halfMoveClock.value
    val ply        = input.fullMoveNumber.ply(sit.color).value
    val brokenTurn = sit.color.black && sit.board(Black, King).isEmpty
    val variantHeader = sit.variant match
      case Standard | Chess960 | FromPosition => 0
      case ThreeCheck                         => 1
      case Antichess                          => 2
      case Atomic                             => 3
      case Horde                              => 4
      case RacingKings                        => 5
      case Crazyhouse                         => 6

    if halfMoves > 0 || ply > 1 || brokenTurn || variantHeader != 0
    then addLeb128(builder, sit.history.halfMoveClock.value)

    if ply > 1 || brokenTurn || variantHeader != 0
    then addLeb128(builder, ply)

    if variantHeader != 0
    then
      builder.addOne(variantHeader.toByte)
      if sit.variant.threeCheck then
        addNibbles(builder, sit.history.checkCount.white, sit.history.checkCount.black)
      else if sit.variant.crazyhouse then
        val crazyData = sit.board.crazyData.getOrElse(Crazyhouse.Data.init)
        addLong(builder, crazyData.promoted.value)
        val pockets = crazyData.pockets
        addNibbles(builder, pockets.white.pawn, pockets.black.pawn)
        addNibbles(builder, pockets.white.knight, pockets.black.knight)
        addNibbles(builder, pockets.white.bishop, pockets.black.bishop)
        addNibbles(builder, pockets.white.rook, pockets.black.rook)
        addNibbles(builder, pockets.white.queen, pockets.black.queen)

    builder.result

  private def addLong(builder: ArrayBuilder[Byte], v: Long) =
    builder.addOne((v >>> 56).toByte)
    builder.addOne((v >>> 48).toByte)
    builder.addOne((v >>> 40).toByte)
    builder.addOne((v >>> 32).toByte)
    builder.addOne((v >>> 24).toByte)
    builder.addOne((v >>> 16).toByte)
    builder.addOne((v >>> 8).toByte)
    builder.addOne(v.toByte)

  private def readLong(reader: Iterator[Byte]): Long =
    (reader.next.toLong << 56) |
      (reader.next.toLong << 48) |
      (reader.next.toLong << 40) |
      (reader.next.toLong << 32) |
      (reader.next.toLong << 24) |
      (reader.next.toLong << 16) |
      (reader.next.toLong << 8) |
      reader.next.toLong

  private def addLeb128(builder: ArrayBuilder[Byte], v: Int) =
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

  private def addNibbles(builder: ArrayBuilder[Byte], lo: Int, hi: Int) =
    builder.addOne((lo | (hi << 4)).toByte)

  private def readNibbles(reader: Iterator[Byte]): (Int, Int) =
    val b = reader.next
    ((b & 0xf), (b >> 4))
