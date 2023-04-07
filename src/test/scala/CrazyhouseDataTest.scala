package chess

import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import Arbitraries.given
import chess.bitboard.Arbitraries.given
import chess.variant.Crazyhouse.Data

class CrazyhouseDataTest extends ScalaCheckSuite:

  override def scalaCheckInitialSeed = "cyiY-djH6CzUsZN2_-G8vcJyBXpkZbaw4MJ1gs1rDaE="

  property("store a piece and drop it"):
    forAll { (piece: Piece, pos: Pos) =>
      (piece.role != King) ==> Data.init.store(piece, pos).drop(!piece).isDefined
    }

  property("store a promoted piece and drop it"):
    forAll { (piece: Piece, pos: Pos) =>
      (piece.role != King && piece.role != Pawn) ==>
        Data.init.promote(pos).store(piece, pos).drop(!piece).isEmpty
    }

  property("store a promoted piece and drop Pawn"):
    forAll { (piece: Piece, pos: Pos) =>
      (piece.role != King && piece.role != Pawn) ==>
        Data.init.promote(pos).store(piece, pos).drop(!piece.color.pawn).isDefined
    }

  property("move a promoted piece and drop Pawn"):
    forAll { (piece: Piece, from: Pos, to: Pos) =>
      (piece.role != King) ==>
        Data.init
          .promote(from)
          .move(from, to)
          .store(piece, to)
          .drop(!piece.color.pawn)
          .isDefined
    }

  property("store and drop multiple pieces"):
    forAll { (ps: List[Piece], pos: Pos) =>
      val data = ps.foldLeft(Data.init) { (data, piece) =>
        data.store(piece, pos)
      }
      val result = ps
        .filter(_.role != King)
        .foldLeft(Option(data)) { (data, piece) =>
          data.flatMap(_.drop(!piece))
        }
      result.isDefined && result.get.isEmpty
    }

  property("store and drop multiple pieces with promotion"):
    forAll { (ps: List[Piece], pos: Pos, promoted: Set[Pos]) =>
      val filtered = ps.filter(_.role != King)
      val data = filtered.foldLeft(Data.init.copy(promoted = promoted)) { (data, piece) =>
        data.store(piece, pos)
      }
      assertEquals(data.size, filtered.size)
    }
