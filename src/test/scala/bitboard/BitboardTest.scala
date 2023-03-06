package chess
package bitboard

import munit.ScalaCheckSuite
import org.lichess.compression.game.Bitboard as CBB
import org.scalacheck.Prop
import Pos.*

import Arbitraries.given

class BitboardTest extends ScalaCheckSuite:

  import scala.language.implicitConversions
  given Conversion[Pos, Int]       = _.value
  given Conversion[Bitboard, Long] = _.value
  import Bitboard.*

  val allDeltas =
    List(
      KING_DELTAS,
      KNIGHT_DELTAS,
      BISHOP_DELTAS,
      ROOK_DELTAS,
      KING_DELTAS,
      WHITE_PAWN_DELTAS,
      BLACK_PAWN_DELTAS
    )

  def as(array: Array[Int]) = array.map(i => s"$i, ").mkString

  test("single cases") {
    val occupied = 20292374.bb
    val deltas   = KING_DELTAS
    val s        = Pos.at(43).get
    val result   = Bitboard.slidingAttacks(s, occupied, deltas)
    val expected = CBB.slidingAttacks(s, occupied, deltas)
    assertEquals(result, expected.bb)
  }

  test("slidingAttack with all occupied") {
    val occupied = Bitboard(-1L)
    for
      i      <- 0 to 63
      deltas <- allDeltas
      s        = Pos.at(i).get
      result   = Bitboard.slidingAttacks(s, occupied, deltas)
      expected = CBB.slidingAttacks(s, occupied, deltas)
    yield assertEquals(result, expected.bb)
  }

  property("slidingAttack check") {
    Prop.forAll { (occupied: Bitboard, s: Pos) =>
      val result = for
        deltas <- allDeltas
        result   = Bitboard.slidingAttacks(s, occupied, deltas)
        expected = CBB.slidingAttacks(s, occupied, deltas)
      yield result == expected.bb
      result.forall(identity)
    }
  }

  test("init function") {
    assertEquals(Bitboard.KNIGHT_ATTACKS.toSeq, CBB.KNIGHT_ATTACKS.toSeq)
    assertEquals(Bitboard.WHITE_PAWN_ATTACKS.toSeq, CBB.WHITE_PAWN_ATTACKS.toSeq)
    assertEquals(Bitboard.BLACK_PAWN_ATTACKS.toSeq, CBB.BLACK_PAWN_ATTACKS.toSeq)
    assertEquals(Bitboard.ATTACKS.toSeq, CBB.ATTACKS.toSeq)
    (0 until 64).foreach { i =>
      assertEquals(Bitboard.RAYS(i).toSeq, CBB.RAYS(i).toSeq)
      assertEquals(Bitboard.BETWEEN(i).toSeq, CBB.BETWEEN(i).toSeq)
    }
  }

  property("bishop attacks") {
    Prop.forAll { (occupied: Bitboard, s: Pos) =>
      s.bishopAttacks(occupied) == CBB.bishopAttacks(s, occupied).bb
    }
  }

  test("special bishop attacks") {
    val result = F8.bishopAttacks(G4.bb).nonEmpty
    assertEquals(result, false)
  }

  property("rook attacks") {
    Prop.forAll { (occupied: Bitboard, s: Pos) =>
      s.rookAttacks(occupied) == CBB.rookAttacks(s, occupied).bb
    }
  }

  property("queen attacks") {
    Prop.forAll { (occupied: Bitboard, s: Pos) =>
      s.queenAttacks(occupied) == CBB.queenAttacks(s, occupied).bb
    }
  }

  property("pawn attacks") {
    Prop.forAll { (s: Pos) =>
      s.pawnAttacks(Color.White) == CBB.pawnAttacks(true, s).bb
      s.pawnAttacks(Color.Black) == CBB.pawnAttacks(false, s).bb
    }
  }

  test("count") {
    assertEquals(1024L.bb.count, 1)
    assertEquals(4264L.bb.count, 4)
    assertEquals((1L.bb & Pos.A1.bb).nonEmpty, true)
    assertEquals(Castles(1L).can(White), true)
  }
