package chess

import cats.syntax.all.*
import chess.variant.Crazyhouse.Data
import chess.bitboard.Bitboard

import munit.ScalaCheckSuite
import org.scalacheck.Prop.{ forAll, propBoolean }
import CoreArbitraries.given

class CrazyhouseDataTest extends ScalaCheckSuite:

  property("store a piece and drop it"):
    forAll: (piece: Piece, square: Square) =>
      (piece.role != King) ==> Data.init.store(piece, square).drop(!piece).isDefined

  property("store a promoted piece and drop it"):
    forAll: (piece: Piece, square: Square) =>
      (piece.role != King && piece.role != Pawn) ==>
        Data.init.promote(square).store(piece, square).drop(!piece).isEmpty

  property("store a promoted piece and drop Pawn"):
    forAll: (piece: Piece, square: Square) =>
      (piece.role != King && piece.role != Pawn) ==>
        Data.init.promote(square).store(piece, square).drop(!piece.color.pawn).isDefined

  property("move a promoted piece and drop Pawn"):
    forAll: (piece: Piece, from: Square, to: Square) =>
      (piece.role != King) ==>
        Data.init
          .promote(from)
          .move(from, to)
          .store(piece, to)
          .drop(!piece.color.pawn)
          .isDefined

  property("store and drop multiple pieces"):
    forAll: (ps: List[Piece], square: Square) =>
      val data = ps.foldLeft(Data.init) { (data, piece) =>
        data.store(piece, square)
      }
      val result = ps
        .filter(_.role != King)
        .foldLeft(Option(data)) { (data, piece) =>
          data.flatMap(_.drop(!piece))
        }
      result.isDefined && result.get.isEmpty

  property("store and drop multiple pieces with promotion"):
    forAll: (ps: List[Piece], square: Square, promoted: Bitboard) =>
      val filtered = ps.filter(_.role != King)
      val data = filtered.foldLeft(Data.init.copy(promoted = promoted)) { (data, piece) =>
        data.store(piece, square)
      }
      assertEquals(data.size, filtered.size)
