package chess
package mateless

/** Mateless's lock detector: pawns, kings and bishops form a structure that no series of legal
  * moves can open, so the flagged side can never be mated (FIDE 5.2.2, 6.9).
  *
  * It reads the bitboards only: it generates no move and searches nothing. Each king's reach (the
  * squares it may ever stand on) is grown from its square, never onto a square that a pawn which
  * can never move attacks; the pawns that can never move are those blocked by a wall, a pawn the
  * other side can never take; the squares where a side may take a pawn grow from its king's reach;
  * and so on to a fixed point. The structure leaks if a pawn can promote or capture, a frozen king
  * can move, a bishop can reach a pawn's path, or the flagged king can reach a square the other
  * side attacks where it has no guaranteed flight. It is tried with both kings free, then with each
  * king frozen on its square (a wall for both sides) when no square around it is free.
  *
  * A knight, rook or queen on the board, two bishops of one side on squares of one colour, or an
  * en passant square fails it at once. Sound, not complete: a structure it passes can never be
  * opened; one it fails may still be dead.
  *
  * Squares as in scalachess (a1 = 0, h8 = 63): the sets are the raw Longs of chess.Bitboard.
  */
object Lock:

  private inline val FileA = 0x0101010101010101L
  private inline val FileH = 0x8080808080808080L
  private inline val Rank1 = 0xffL
  private inline val Rank8 = 0xff00000000000000L
  private inline val Dark = 0xaa55aa55aa55aa55L

  private inline def north(w: Long): Long = w << 8
  private inline def south(w: Long): Long = w >>> 8
  private inline def sideways(w: Long): Long = ((w << 1) & ~FileA) | ((w >>> 1) & ~FileH)
  private inline def zone(w: Long): Long =
    val v = w | (w << 8) | (w >>> 8)
    v | sideways(v)
  private inline def diagonal(w: Long): Long = sideways((w << 8) | (w >>> 8))
  private inline def count(w: Long): Int = java.lang.Long.bitCount(w)

  /** Every square reachable from g by steps north (south) through squares of p. */
  private inline def fillNorth(g0: Long, p0: Long): Long =
    var g = g0
    var p = p0
    g |= p & (g << 8)
    p &= p << 8
    g |= p & (g << 16)
    p &= p << 16
    g | (p & (g << 32))
  private inline def fillSouth(g0: Long, p0: Long): Long =
    var g = g0
    var p = p0
    g |= p & (g >>> 8)
    p &= p >>> 8
    g |= p & (g >>> 16)
    p &= p >>> 16
    g | (p & (g >>> 32))

  private inline def twinBishops(b: Long): Boolean = count(b & Dark) > 1 || count(b & ~Dark) > 1

  /** Can `flagged` never be mated, by the lock detector? */
  def cannotBeMated(pos: Position, flagged: Color): Boolean =
    val b = pos.board
    if (b.knights.value | b.rooks.value | b.queens.value) != 0 then false
    else
      val wb = (b.bishops & b.white).value
      val bb = (b.bishops & b.black).value
      if twinBishops(wb) || twinBishops(bb) || pos.enPassantSquare.isDefined then false
      else
        val wp = (b.pawns & b.white).value
        val bp = (b.pawns & b.black).value
        val wk = (b.kings & b.white).value
        val bk = (b.kings & b.black).value
        !leaks(wp, bp, wk, bk, wb, bb, flagged, 0L) || {
          val held = wp | bp | sideways(north(wp) | south(bp))
          ((zone(wk) & ~wk & ~held) == 0 && !leaks(wp, bp, wk, bk, wb, bb, flagged, wk)) ||
          ((zone(bk) & ~bk & ~held) == 0 && !leaks(wp, bp, wk, bk, wb, bb, flagged, bk))
        }

  /** Does the structure leak to the flagged king, with king `fr` frozen (0: none)? */
  private def leaks(
      wp: Long,
      bp: Long,
      wk: Long,
      bk: Long,
      wb: Long,
      bb: Long,
      flagged: Color,
      fr: Long
  ): Boolean =
    val frozenWhite = (fr & wk) != 0
    val frozenBlack = !frozenWhite && fr != 0
    val nearFrozenBlack = if frozenBlack then zone(bk) else 0L
    val nearFrozenWhite = if frozenWhite then zone(wk) else 0L
    // Quick reject with the first walls (the capture squares are only the kings' squares yet).
    val wc0 = fillNorth(wp, ~((bp & ~wk) | fr))
    val bc0 = fillSouth(bp, ~((wp & ~bk) | fr))
    if ((wc0 & Rank8) | (bc0 & Rank1)) != 0 || (north(sideways(wc0)) & bc0) != 0 then true
    else
      // The cycle: walls -> pawns that can never move -> king reaches -> capture squares -> walls.
      var wFixed = wp
      var bFixed = bp
      var wKing = wk
      var bKing = bk
      var wTake = wk
      var bTake = bk
      var wWall = -1L
      var bWall = -1L
      var leak = false
      var done = false
      while !done do
        val pw = wWall
        val pb = bWall
        wWall = (wp & ~bTake) | fr
        bWall = (bp & ~wTake) | fr
        val nwFixed = wFixed & wWall & south(bWall | wFixed)
        val nbFixed = bFixed & bWall & north(wWall | bFixed)
        if wWall == pw && bWall == pb && nwFixed == wFixed && nbFixed == bFixed then done = true
        else
          // Both kings' reaches, flooded against their masks.
          val mw = ~(nwFixed | sideways(south(nbFixed)) | nearFrozenBlack)
          val mb = ~(nbFixed | sideways(north(nwFixed)) | nearFrozenWhite)
          var nwKing = wKing
          var nbKing = bKing
          var stable = false
          while !stable do
            val a = nwKing | (zone(nwKing) & mw)
            val d = nbKing | (zone(nbKing) & mb)
            if a == nwKing && d == nbKing then stable = true
            else
              nwKing = a
              nbKing = d
          if (frozenWhite && nwKing != wk) || (frozenBlack && nbKing != bk) then
            leak = true // the frozen king can move
            done = true
          else
            val tb = bTake | nbKing
            val nbTake = tb | south(fillSouth(tb & ~bWall, ~bWall))
            val tw = wTake | nwKing
            val nwTake = tw | north(fillNorth(tw & ~wWall, ~wWall))
            val changed = (nwFixed ^ wFixed) | (nbFixed ^ bFixed) | (nwKing ^ wKing) | (nbKing ^ bKing) |
              (nbTake ^ bTake) | (nwTake ^ wTake)
            wFixed = nwFixed
            bFixed = nbFixed
            wKing = nwKing
            bKing = nbKing
            wTake = nwTake
            bTake = nbTake
            if changed == 0 then done = true
      if leak then true
      else
        // The pawns' cones (the squares they may still reach) and the bishops' reaches, once.
        val wCone = fillNorth(wp, ~bWall)
        val bCone = fillSouth(bp, ~wWall)
        if ((wCone & Rank8) | (bCone & Rank1)) != 0 then true // a pawn can promote
        else if (north(sideways(wCone)) & bCone) != 0 then true // a pawn can capture
        else
          var wBish = wb
          var bBish = bb
          if (wBish | bBish) != 0 then
            val m = ~(wFixed | bFixed)
            var stable = false
            while !stable do
              val a = wBish | (diagonal(wBish) & m)
              val d = bBish | (diagonal(bBish) & m)
              if a == wBish && d == bBish then stable = true
              else
                wBish = a
                bBish = d
          if (diagonal(wBish) & bCone) != 0 || (diagonal(bBish) & wCone) != 0 then
            true // a bishop meets a pawn's path
          else
            // The mating net: can the flagged king be caught by the other side's pawns and bishops?
            val attack =
              if flagged.white then diagonal(bBish) | sideways(south(bCone))
              else diagonal(wBish) | sideways(north(wCone))
            val reach = if flagged.white then wKing else bKing
            val free =
              if flagged.white then ~(wCone | zone(bKing) | attack) else ~(bCone | zone(wKing) | attack)
            val spare = free & ~(if flagged.white then wBish else bBish)
            val flight =
              sideways(spare) | north(spare) | south(spare) | ((north(free) | south(free)) & sideways(free))
            (attack & reach & ~flight) != 0
