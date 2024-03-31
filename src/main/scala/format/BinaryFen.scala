package chess
package format

import scala.collection.mutable.ArrayBuilder

opaque type BinaryFen = Array[Byte]

object BinaryFen:
  extension (bf: BinaryFen)
    def read: Situation =
      val fused = new Iterator[Byte]:
        val inner                   = bf.iterator
        inline def hasNext: Boolean = inner.hasNext
        inline def next: Byte       = if hasNext then inner.next else 0.toByte
      ???

  def write(input: Situation.AndFullMoveNumber): BinaryFen =
    val builder = ArrayBuilder.ofByte()

    val sit      = input.situation
    val occupied = sit.board.occupied
    addLong(builder, occupied.value)

    val pawnPushedTo: Option[Square] = sit.enPassantSquare.map(_.xor(Square.A2))

    def compressPiece(sq: Square): Byte =
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
    do addNibbles(builder, compressPiece(it.next), if it.hasNext then compressPiece(it.next) else 0)

    val halfMoves = sit.history.halfMoveClock.value

    val ply        = input.fullMoveNumber.ply(sit.color).value
    val brokenTurn = sit.color.black && sit.board(Black, King).isEmpty
    val variantHeader = sit.variant match
      case variant.Standard | variant.Chess960 | variant.FromPosition => 0
      case variant.ThreeCheck                                         => 1
      case variant.Antichess                                          => 2
      case variant.Atomic                                             => 3
      case variant.Horde                                              => 4
      case variant.RacingKings                                        => 5
      case variant.Crazyhouse                                         => 6

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
        val crazyData = sit.board.crazyData.getOrElse(variant.Crazyhouse.Data.init)
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

  private def addLeb128(builder: ArrayBuilder[Byte], v: Int) =
    var n = v
    while n > 127
    do
      builder.addOne((n | 128).toByte)
      n = n >>> 7
    builder.addOne(n.toByte)

  private def addNibbles(builder: ArrayBuilder[Byte], lo: Int, hi: Int) =
    builder.addOne((lo | (hi << 4)).toByte)
