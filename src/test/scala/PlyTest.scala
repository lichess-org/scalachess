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
      Ply(0).fullMoveNumber === FullMoveNumber(1) // root
      Ply(1).fullMoveNumber === FullMoveNumber(1) // e4
      Ply(2).fullMoveNumber === FullMoveNumber(2) // e5
      Ply(3).fullMoveNumber === FullMoveNumber(2) // f4
      Ply(4).fullMoveNumber === FullMoveNumber(3) // Nf6
      Ply(5).fullMoveNumber === FullMoveNumber(3)
      Ply(6).fullMoveNumber === FullMoveNumber(4)
    "from full move number" in:
      FullMoveNumber(1).ply(White) === Ply(0)
      FullMoveNumber(1).ply(Black) === Ply(1)
      FullMoveNumber(2).ply(White) === Ply(2)
      FullMoveNumber(2).ply(Black) === Ply(3)
      FullMoveNumber(3).ply(White) === Ply(4)
      FullMoveNumber(3).ply(Black) === Ply(5)
