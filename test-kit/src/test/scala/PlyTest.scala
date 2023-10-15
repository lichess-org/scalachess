package chess

// https://www.chessprogramming.org/Forsyth-Edwards_Notation#Fullmove_counter
// ply FEN (ends with full move number)
// 0   rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
// 1   rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
// 2   rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2
// 3   rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

class PlyTest extends ChessTest:

  test("to full move number"):
    assertEquals(Ply(0).fullMoveNumber, FullMoveNumber(1)) // root
    assertEquals(Ply(1).fullMoveNumber, FullMoveNumber(1)) // e4
    assertEquals(Ply(2).fullMoveNumber, FullMoveNumber(2)) // e5
    assertEquals(Ply(3).fullMoveNumber, FullMoveNumber(2)) // f4
    assertEquals(Ply(4).fullMoveNumber, FullMoveNumber(3)) // Nf6
    assertEquals(Ply(5).fullMoveNumber, FullMoveNumber(3))
    assertEquals(Ply(6).fullMoveNumber, FullMoveNumber(4))

  test("from full move number"):
    assertEquals(FullMoveNumber(1).ply(White), Ply(0))
    assertEquals(FullMoveNumber(1).ply(Black), Ply(1))
    assertEquals(FullMoveNumber(2).ply(White), Ply(2))
    assertEquals(FullMoveNumber(2).ply(Black), Ply(3))
    assertEquals(FullMoveNumber(3).ply(White), Ply(4))
    assertEquals(FullMoveNumber(3).ply(Black), Ply(5))
