package chess
package bitboard

import scala.annotation.static

class Attacks
object Attacks:
  @static
  private val all = -1L

  @static
  private[chess] val RANKS = Array.fill(8)(0L)
  @static
  private[chess] val FILES = Array.fill(8)(0L)
  @static
  private[chess] val BETWEEN = Array.ofDim[Long](64, 64)
  @static
  private[chess] val RAYS = Array.ofDim[Long](64, 64)

  // Large overlapping attack table indexed using magic multiplication.
  @static
  private[chess] val ATTACKS = Array.fill(88772)(0L)
  @static
  private[chess] val KNIGHT_ATTACKS = Array.fill(64)(0L)
  @static
  private[chess] val KING_ATTACKS = Array.fill(64)(0L)
  @static
  private[chess] val WHITE_PAWN_ATTACKS = Array.fill(64)(0L)
  @static
  private[chess] val BLACK_PAWN_ATTACKS = Array.fill(64)(0L)

  @static
  private val KNIGHT_DELTAS = Array[Int](17, 15, 10, 6, -17, -15, -10, -6)
  @static
  private val BISHOP_DELTAS = Array[Int](7, -7, 9, -9)
  @static
  private val ROOK_DELTAS = Array[Int](1, -1, 8, -8)
  @static
  private val KING_DELTAS = Array[Int](1, 7, 8, 9, -1, -7, -8, -9)
  @static
  private val WHITE_PAWN_DELTAS = Array[Int](7, 9)
  @static
  private val BLACK_PAWN_DELTAS = Array[Int](-7, -9)

  /** Slow attack set generation. Used only to bootstrap the attack tables.
    */
  private def slidingAttacks(square: Int, occupied: Long, deltas: Array[Int]): Long =
    var attacks = 0L
    deltas.foreach: delta =>
      var sq = square
      while
        sq += delta
        val con = (sq < 0 || 64 <= sq || distance(sq, sq - delta) > 2)
        if !con then attacks |= 1L << sq

        !(occupied.contains(sq) || con)
      do ()
    attacks

  private def initMagics(square: Int, magic: Magic, shift: Int, deltas: Array[Int]) =
    var subset = 0L
    while
      val attack = slidingAttacks(square, subset, deltas)
      val idx    = ((magic.factor * subset) >>> (64 - shift)).toInt + magic.offset
      ATTACKS(idx) = attack

      // Carry-rippler trick for enumerating subsets.
      subset = (subset - magic.mask) & magic.mask

      subset != 0
    do ()

  private def initialize() =
    (0 until 8).foreach: i =>
      RANKS(i) = 0xffL << (i * 8)
      FILES(i) = 0x0101010101010101L << i

    val squareRange = 0 until 64
    squareRange.foreach: sq =>
      KNIGHT_ATTACKS(sq) = slidingAttacks(sq, all, KNIGHT_DELTAS)
      KING_ATTACKS(sq) = slidingAttacks(sq, all, KING_DELTAS)
      WHITE_PAWN_ATTACKS(sq) = slidingAttacks(sq, all, WHITE_PAWN_DELTAS)
      BLACK_PAWN_ATTACKS(sq) = slidingAttacks(sq, all, BLACK_PAWN_DELTAS)

      initMagics(sq, Magic.ROOK(sq), 12, ROOK_DELTAS)
      initMagics(sq, Magic.BISHOP(sq), 9, BISHOP_DELTAS)

    for
      a <- squareRange
      b <- squareRange
      _ =
        if slidingAttacks(a, 0, ROOK_DELTAS).contains(b) then
          BETWEEN(a)(b) = slidingAttacks(a, 1L << b, ROOK_DELTAS) & slidingAttacks(b, 1L << a, ROOK_DELTAS)
          RAYS(a)(b) =
            (1L << a) | (1L << b) | slidingAttacks(a, 0, ROOK_DELTAS) & slidingAttacks(b, 0, ROOK_DELTAS)
        else if slidingAttacks(a, 0, BISHOP_DELTAS).contains(b) then
          BETWEEN(a)(b) =
            slidingAttacks(a, 1L << b, BISHOP_DELTAS) & slidingAttacks(b, 1L << a, BISHOP_DELTAS)
          RAYS(a)(b) =
            (1L << a) | (1L << b) | slidingAttacks(a, 0, BISHOP_DELTAS) & slidingAttacks(b, 0, BISHOP_DELTAS)
    yield ()

  val _ = initialize()

  extension (l: Long)
    private def contains(s: Int): Boolean =
      (l & (1L << s)) != 0L

  private def distance(a: Int, b: Int): Int =
    inline def file(s: Int) = s & 7
    inline def rank(s: Int) = s >>> 3
    Math.max(Math.abs(file(a) - file(b)), Math.abs(rank(a) - rank(b)))
