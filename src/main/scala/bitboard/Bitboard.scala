package chess
package bitboard

opaque type Bitboard = Long
object Bitboard extends OpaqueBitboard[Bitboard]:

  private val RANKS = Array.fill(8)(0L)
  private val FILES = Array.fill(8)(0L)

  val firstRank = Bitboard(0xffL)
  val lastRank  = Bitboard(0xffL << 56)

  // all light squares
  val lightSquares = Bitboard(0x55aa55aa55aa55aaL)
  // all dark squares
  val darkSquares = Bitboard(0xaa55aa55aa55aa55L)

  private[bitboard] val KNIGHT_DELTAS     = Array[Int](17, 15, 10, 6, -17, -15, -10, -6)
  private[bitboard] val BISHOP_DELTAS     = Array[Int](7, -7, 9, -9)
  private[bitboard] val ROOK_DELTAS       = Array[Int](1, -1, 8, -8)
  private[bitboard] val KING_DELTAS       = Array[Int](1, 7, 8, 9, -1, -7, -8, -9)
  private[bitboard] val WHITE_PAWN_DELTAS = Array[Int](7, 9)
  private[bitboard] val BLACK_PAWN_DELTAS = Array[Int](-7, -9)

  private[bitboard] val KNIGHT_ATTACKS     = Array.fill(64)(0L)
  private[bitboard] val KING_ATTACKS       = Array.fill(64)(0L)
  private[bitboard] val WHITE_PAWN_ATTACKS = Array.fill(64)(0L)
  private[bitboard] val BLACK_PAWN_ATTACKS = Array.fill(64)(0L)

  private[bitboard] val BETWEEN = Array.ofDim[Long](64, 64)
  private[bitboard] val RAYS    = Array.ofDim[Long](64, 64)

  // Large overlapping attack table indexed using magic multiplication.
  private[bitboard] val ATTACKS = Array.fill(88772)(0L)

  inline def rank(inline r: Rank): Bitboard                        = RANKS(r.value)
  inline def file(inline f: File): Bitboard                        = FILES(f.value)
  inline def ray(inline from: Square, inline to: Square): Bitboard = RAYS(from.value)(to.value)

  /** Slow attack set generation. Used only to bootstrap the attack tables.
    */
  private[bitboard] def slidingAttacks(square: Int, occupied: Bitboard, deltas: Array[Int]): Bitboard =
    var attacks = 0L
    deltas.foreach { delta =>
      var sq: Int = square
      var i       = 0
      while
        i += 1
        sq += delta
        val con = (sq < 0 || 64 <= sq || distance(sq, sq - delta) > 2)
        if (!con)
          attacks |= 1L << sq

        !(occupied.contains(Square(sq)) || con)
      do ()
    }
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
    (0 until 8).foreach { i =>
      RANKS(i) = 0xffL << (i * 8)
      FILES(i) = 0x0101010101010101L << i
    }

    val squareRange = 0 until 64
    squareRange.foreach { sq =>
      KNIGHT_ATTACKS(sq) = slidingAttacks(sq, Bitboard.ALL, KNIGHT_DELTAS)
      KING_ATTACKS(sq) = slidingAttacks(sq, Bitboard.ALL, KING_DELTAS)
      WHITE_PAWN_ATTACKS(sq) = slidingAttacks(sq, Bitboard.ALL, WHITE_PAWN_DELTAS)
      BLACK_PAWN_ATTACKS(sq) = slidingAttacks(sq, Bitboard.ALL, BLACK_PAWN_DELTAS)

      initMagics(sq, Magic.ROOK(sq), 12, ROOK_DELTAS)
      initMagics(sq, Magic.BISHOP(sq), 9, BISHOP_DELTAS)
    }

    for
      a <- squareRange
      b <- squareRange
      _ =
        if slidingAttacks(a, 0, ROOK_DELTAS).contains(Square(b)) then
          BETWEEN(a)(b) = slidingAttacks(a, 1L << b, ROOK_DELTAS) & slidingAttacks(b, 1L << a, ROOK_DELTAS)
          RAYS(a)(b) =
            (1L << a) | (1L << b) | slidingAttacks(a, 0, ROOK_DELTAS) & slidingAttacks(b, 0, ROOK_DELTAS)
        else if slidingAttacks(a, 0, BISHOP_DELTAS).contains(Square(b)) then
          BETWEEN(a)(b) =
            slidingAttacks(a, 1L << b, BISHOP_DELTAS) & slidingAttacks(b, 1L << a, BISHOP_DELTAS)
          RAYS(a)(b) =
            (1L << a) | (1L << b) | slidingAttacks(a, 0, BISHOP_DELTAS) & slidingAttacks(b, 0, BISHOP_DELTAS)
    yield ()

  initialize()

  def aligned(a: Square, b: Square, c: Square): Boolean =
    ray(a, b).contains(c)

  def between(a: Square, b: Square): Bitboard =
    BETWEEN(a.value)(b.value)

  extension (s: Square)
    def bishopAttacks(occupied: Bitboard): Bitboard =
      val magic = Magic.BISHOP(s.value)
      ATTACKS(((magic.factor * (occupied & magic.mask) >>> (64 - 9)).toInt + magic.offset))

    def rookAttacks(occupied: Bitboard): Bitboard =
      val magic = Magic.ROOK(s.value)
      ATTACKS(((magic.factor * (occupied & magic.mask) >>> (64 - 12)).toInt + magic.offset))

    def queenAttacks(occupied: Bitboard): Bitboard =
      bishopAttacks(occupied) ^ rookAttacks(occupied)

    def pawnAttacks(color: Color): Bitboard =
      color match
        case Color.White => WHITE_PAWN_ATTACKS(s.value)
        case Color.Black => BLACK_PAWN_ATTACKS(s.value)

    def kingAttacks: Bitboard =
      KING_ATTACKS(s.value)

    def knightAttacks: Bitboard =
      KNIGHT_ATTACKS(s.value)

  private def distance(a: Int, b: Int): Int =
    inline def file(p: Int) = p & 7
    inline def rank(p: Int) = p >>> 3
    Math.max(Math.abs(file(a) - file(b)), Math.abs(rank(a) - rank(b)))
