package chess

// https://www.chessprogramming.org/Forsyth-Edwards_Notation#Fullmove_counter
// ply FEN (ends with full move number)
// 0   rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
// 1   rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
// 2   rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2
// 3   rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2
class PlyTest extends ChessTest:

  "plies" should:
    "to full move number" in:
      Ply.from((0 to 6).toList).map(ply => ply -> ply.fullMoveNumber) must_== List(
        Ply(0) -> FullMoveNumber(1), // root
        Ply(1) -> FullMoveNumber(1), // e4
        Ply(2) -> FullMoveNumber(2), // e5
        Ply(3) -> FullMoveNumber(2), // f4
        Ply(4) -> FullMoveNumber(3), // Nf6
        Ply(5) -> FullMoveNumber(3),
        Ply(6) -> FullMoveNumber(4)
      )
    "from full move number" in:
      val set = for
        fmn  <- FullMoveNumber.from((1 to 3).toList)
        turn <- Color.all
      yield (fmn, turn, fmn.ply(turn))
      set must_== List(
        (FullMoveNumber(1), White, Ply(0)),
        (FullMoveNumber(1), Black, Ply(1)),
        (FullMoveNumber(2), White, Ply(2)),
        (FullMoveNumber(2), Black, Ply(3)),
        (FullMoveNumber(3), White, Ply(4)),
        (FullMoveNumber(3), Black, Ply(5))
      )
