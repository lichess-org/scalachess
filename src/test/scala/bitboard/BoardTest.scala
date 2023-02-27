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

  test("sliderBlockers") {
    for
      fen <- FenFixtures.fens
      situation = Fen.read(fen).getOrElse(throw RuntimeException("boooo"))
      color <- Color.all
      result   = situation.sliderBlockers
      king     = situation.ourKings.head
      expected = situation.cBoard.sliderBlockers(king)
    yield assertEquals(result, expected.bb)
  }

  test("attacksTo") {
    for
      str <- FenFixtures.fens
      fen  = Fen.read(str).getOrElse(throw RuntimeException("boooo"))
      king = fen.ourKings.head
      sq    <- Pos.all
      color <- List(Color.White, Color.Black)
      result   = fen.board.attackers(sq, color)
      expected = fen.cBoard.attacksTo(sq, color.white)
    yield assertEquals(result, expected.bb)
  }

  test("discard a empty square returns the same board") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if !board.hasPiece(s)
      newBoard = board.discard(s)
    yield assert(newBoard == board)
  }

  test("discard an occupied square returns a board with one piece left") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      if board.hasPiece(s)
      newBoard = board.discard(s)
    yield assert(board.hasPiece(s) && newBoard.nbPieces == board.nbPieces - 1)
  }

  test("take return some if the pos is not empty") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      newBoard = board.take(s)
    yield assert(newBoard.isEmpty || (board.hasPiece(s) && newBoard.isDefined && newBoard.get != board))
  }

  test("put returns None if the pos is not empty") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      s <- Pos.all
      newBoard = board.put(Piece(White, King), s)
    yield assert(newBoard.isDefined || (board.hasPiece(s) && newBoard.isEmpty))
  }

  test("discard . put == identity") {
    for
      str <- FenFixtures.fens
      board    = parseFen(str)
      newBoard = board.pieceMap.foldLeft(board)((b, s) => b.discard(s._1).put(s._2, s._1).get)
    yield assertEquals(newBoard, board)
  }

  test("putOrReplace for every occupied square returns the same board") {
    for
      str <- FenFixtures.fens
      board  = parseFen(str)
      result = board.pieceMap.foldLeft(Board.empty)((b, s) => b.putOrReplace(s._1, s._2))
    yield assertEquals(result, board)
  }

  test("pieceMap . fromMap == identity") {
    for
      str <- FenFixtures.fens
      board  = parseFen(str)
      result = Board.fromMap(board.pieceMap)
    yield assertEquals(result, board)
  }

  test("pieces.toSet == pieceMap.values.toSet") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
    yield assertEquals(board.pieces.toSet, board.pieceMap.values.toSet)
  }

  test("piecesOf(White) ++ piecesOf(Black) == pieceMap") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      white = board.piecesOf(White)
      black = board.piecesOf(Black)
    yield assertEquals(white ++ black, board.pieceMap)
  }

  test("hasPiece(pos) == pieceMap.contains(pos)") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      pos <- Pos.all
    yield assertEquals(board.hasPiece(pos), board.pieceMap.contains(pos))
  }

  test("hasPiece(piece) == true if pieces contains piece") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      piece <- board.pieces
    yield assert(board.hasPiece(piece))
  }

  test("move(x, x) always return None") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      moved = board.move(from, from)
    yield assert(moved.isEmpty)
  }

  test("move(from, to).isDefined == hasPiece(from) && !hasPiece(to)") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      to   <- Pos.all
      if from != to
      moved = board.move(from, to)
    yield assertEquals(moved.isDefined, board.hasPiece(from) && !board.hasPiece(to))
  }

  test("move(from, to).move(to, from) == identity") {
    for
      str <- FenFixtures.fens
      board = parseFen(str)
      from <- Pos.all
      to   <- Pos.all
      if from != to
      moved     = board.move(from, to)
      movedBack = moved.flatMap(_.move(to, from))
    yield assert(movedBack.isEmpty || movedBack == Some(board))
  }

  test("if from != to then move(from, to) == take(from) . put(to)") {
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
  }

  test("occupation") {
    val map = Map(A2 -> White.pawn, A3 -> White.rook)
    assertEquals(Board.fromMap(map).occupation, Color.Map(map.keys.toSet, Set()))
  }
