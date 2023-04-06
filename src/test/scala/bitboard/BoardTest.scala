package chess
package bitboard

import cats.syntax.all.*
import munit.FunSuite

import Pos.*
import Helpers.*
import Bitboard.*
import chess.variant.Standard
import chess.format.Fen
import chess.format.EpdFen

class BoardTest extends FunSuite:

  import scala.language.implicitConversions
  given Conversion[Pos, Int] = _.value
  given Conversion[Int, Pos] = Pos.at(_).get

  def parseFen(fen: EpdFen): Board =
    Fen.read(fen).map(_.board.board).getOrElse(throw RuntimeException("boooo"))

  test("sliderBlockers"):
    for
      fen <- FenFixtures.fens
      situation = Fen.read(fen).getOrElse(throw RuntimeException("boooo"))
      king      = situation.ourKings.head
      result    = situation.board.sliderBlockers(king, situation.color)
      expected  = situation.cBoard.sliderBlockers(king)
    yield assertEquals(result, expected.bb)

  test("generateMoves = generateMovesAt for all pos"):
    for
      fen <- FenFixtures.fens
      situation     = Fen.read(fen).getOrElse(throw RuntimeException("boooo"))
      legalMoves    = situation.legalMoves
      legalMovesAll = Pos.all.flatMap(situation.generateMovesAt(_))
    yield assertEquals(legalMoves.toSet, legalMovesAll.toSet)

  test("generateMovesAt(pos) = generateMoves.filter(_.orig == pos)"):
    for
      fen <- FenFixtures.fens
      situation = Fen.read(fen).getOrElse(throw RuntimeException("boooo"))
      sq <- Pos.all
      legalMoves   = situation.legalMoves.filter(_.orig == sq)
      legalMovesAt = situation.generateMovesAt(sq)
    yield assertEquals(legalMoves.toSet, legalMovesAt.toSet)

  test("attacksTo"):
    for
      str <- FenFixtures.fens
      fen  = Fen.read(str).getOrElse(throw RuntimeException("boooo"))
      king = fen.ourKings.head
      sq    <- Pos.all
      color <- List(Color.White, Color.Black)
      result   = fen.board.attackers(sq, color)
      expected = fen.cBoard.attacksTo(sq, color.white)
    yield assertEquals(result, expected.bb)

  test("discard an empty square returns the same board"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if !board.isOccupied(s)
      newBoard = board.discard(s)
    yield assertEquals(newBoard, board)

  test("discard an occupied square returns a board with one piece left"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if board.isOccupied(s)
      newBoard = board.discard(s)
    yield assertEquals(newBoard.nbPieces, board.nbPieces - 1)

  test("discard all occupied squares returns an empty board"):
    for
      str <- FenFixtures.fens
      board    = parseFen(str)
      newBoard = board.occupied.fold(board) { (b, s) => b.discard(s) }
    yield assertEquals(newBoard, Board.empty)

  test("take an occupied square returns Some(board.discard)"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if board.isOccupied(s)
      newBoard = board.take(s)
    yield assertEquals(newBoard.get, board.discard(s))

  test("take returns None if the square is empty"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if !board.isOccupied(s)
      newBoard = board.take(s)
    yield assert(newBoard.isEmpty)

  test("put returns None if the square is not empty"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      newBoard = board.put(White.king, s)
    yield assert(newBoard.isDefined || (board.isOccupied(s) && newBoard.isEmpty))

  test("discard . put == identity"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if board.isOccupied(s)
      piece    <- board.pieceAt(s)
      newBoard <- board.discard(s).put(piece, s)
    yield assertEquals(newBoard, board)

  test("replace returns None if the square is empty"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if !board.isOccupied(s)
      newBoard = board.replace(White.king, s)
    yield assert(newBoard.isEmpty)

  test("replace returns some if the square is occupied"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if board.isOccupied(s)
      newBoard = board.replace(White.king, s)
    yield assert(newBoard.isDefined)

  test("put.replace.pieceAt returns the piece that was replace"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if !board.isOccupied(s)
      newPiece = for
        x        <- board.put(White.king, s)
        newBoard <- x.replace(White.queen, s)
        newPiece <- newBoard.pieceAt(s)
      yield newPiece
    yield assertEquals(newPiece.get, White.queen)

  test("putOrReplace an empty square returns a board with one more piece"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if !board.isOccupied(s)
      newBoard = board.putOrReplace(White.queen, s)
    yield assertEquals(newBoard.nbPieces, board.nbPieces + 1)

  test("putOrReplace an occupied square returns a board with the same number of pieces"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if board.isOccupied(s)
      newBoard = board.putOrReplace(White.queen, s)
    yield assertEquals(newBoard.nbPieces, board.nbPieces)

  test("putOrReplace for every occupied square returns the same board"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      result = board.occupied.fold(board) { (b, s) =>
        val piece = b.pieceAt(s).get
        b.putOrReplace(piece, s)
      }
    yield assertEquals(result, board)

  test("putOrReplace == put orElse replace"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      result = board.putOrReplace(White.king, s)
      expected <- board.put(White.king, s) orElse board.replace(White.king, s)
    yield assertEquals(result, expected)

  test("pieceMap . fromMap == identity"):
    for
      str <- FenFixtures.fens
      board  = parseFen(str)
      result = Board.fromMap(board.pieceMap)
    yield assertEquals(result, board)

  test("pieces.toSet == pieceMap.values.toSet"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
    yield assertEquals(board.pieces.toSet, board.pieceMap.values.toSet)

  test("piecesOf(White) ++ piecesOf(Black) == pieceMap"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      white = board.piecesOf(White)
      black = board.piecesOf(Black)
    yield assertEquals(white ++ black, board.pieceMap)

  test("isOccupied(pos) == pieceMap.contains(pos)"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      pos <- Pos.all
    yield assertEquals(board.isOccupied(pos), board.pieceMap.contains(pos))

  test("isOccupied(piece) == true if pieces contains piece"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      piece <- board.pieces
    yield assert(board.isOccupied(piece))

  test("move(x, x) always returns None"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      moved = board.move(from, from)
    yield assert(moved.isEmpty)

  test("move(from, to).isDefined == isOccupied(from) && !isOccupied(to)"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      to   <- Pos.all
      if from != to
      moved = board.move(from, to)
    yield assertEquals(moved.isDefined, board.isOccupied(from) && !board.isOccupied(to))

  test("move(from, to).move(to, from) == identity"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      to   <- Pos.all
      if from != to
      moved     = board.move(from, to)
      movedBack = moved.flatMap(_.move(to, from))
    yield assert(movedBack.isEmpty || movedBack == Some(board))

  test("move(from, to) == take(from) . put(to)"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      to   <- Pos.all
      if from != to
      moved = board.move(from, to)
      takeAndPut = for
        piece     <- board.pieceAt(from)
        afterTake <- board.take(from)
        newBoard  <- afterTake.put(piece, to)
      yield newBoard
    yield assertEquals(moved, takeAndPut)

  test("taking(from, to, None) == take(from) . replace(to)"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      to   <- Pos.all
      if from != to
      taking = board.taking(from, to)
      takeAndReplace = for
        piece     <- board.pieceAt(from)
        afterTake <- board.take(from)
        newBoard  <- afterTake.replace(piece, to)
      yield newBoard
    yield assertEquals(taking, takeAndReplace)

  test("taking(from, to, taken) == take(from) . put(to) . take(taken)"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from  <- Pos.all
      to    <- Pos.all
      taken <- Pos.all
      if taken != from && from != to && taken != to
      taking <- board.taking(from, to, Some(taken))
      takeAndPutAndTake <- for
        piece     <- board.pieceAt(from)
        afterTake <- board.take(from)
        afterPut  <- afterTake.put(piece, to)
        newBoard  <- afterPut.take(taken)
      yield newBoard
    yield assertEquals(taking, takeAndPutAndTake)

  test("promote(from, to, piece) == move(from, to).replace(piece, to) when to is empty"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      to   <- Pos.all
      if from != to && !board.isOccupied(to)
      piece    = White.knight
      promoted = board.promote(from, to, piece)
      moveAndReplace = for
        moved    <- board.move(from, to)
        newBoard <- moved.replace(piece, to)
      yield newBoard
    yield assertEquals(promoted, moveAndReplace)

  test("promote(from, to, piece) == taking(from, to).put(piece, to) when to is occupied"):
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      to   <- Pos.all
      if from != to && board.isOccupied(to)
      piece    = White.knight
      promoted = board.promote(from, to, piece)
      takingAndReplace = for
        moved    <- board.taking(from, to)
        newBoard <- moved.replace(piece, to)
      yield newBoard
    yield assertEquals(promoted, takingAndReplace)

  test("occupation"):
    val map = Map(A2 -> White.pawn, A3 -> White.rook)
    assertEquals(Board.fromMap(map).occupation, Color.Map(map.keys.toSet, Set()))
