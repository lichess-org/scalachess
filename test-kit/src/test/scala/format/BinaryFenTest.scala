package chess
package format

import chess.bitboard.FenFixtures
import chess.variant.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.collection.mutable.ArrayBuilder

class BinaryFenTest extends ScalaCheckSuite:
  test("long roundtrip"):
    forAll: (v: Long) =>
      val builder = ArrayBuilder.ofByte()
      BinaryFen.implementation.writeLong(builder, v)
      assertEquals(BinaryFen.implementation.readLong(builder.result.iterator), v)

  test("leb128 roundtrip"):
    forAll(Gen.posNum[Int]): (v: Int) =>
      val builder = ArrayBuilder.ofByte()
      BinaryFen.implementation.writeLeb128(builder, v)
      assertEquals(BinaryFen.implementation.readLeb128(builder.result.iterator), v)

  private val genNibble = Gen.chooseNum[Int](0, 15)

  test("nibbles roundtrip"):
    forAll(genNibble, genNibble): (lo: Int, hi: Int) =>
      val builder = ArrayBuilder.ofByte()
      BinaryFen.implementation.writeNibbles(builder, lo, hi)
      assertEquals(BinaryFen.implementation.readNibbles(builder.result.iterator), (lo, hi))

  test("rewrite fixpoint"):
    forAll: (bytes: Array[Byte]) =>
      val rewritten = BinaryFen.write(BinaryFen(bytes).read)
      assertEquals(BinaryFen.write(rewritten.read), rewritten)

  test("handpicked fens roundtrip"):
    assertRoundtrip(Standard, FullFen("8/8/8/8/8/8/8/8 w - - 0 1"))
    assertRoundtrip(Standard, FullFen("8/8/8/8/8/8/8/8 b - - 0 1"))
    assertRoundtrip(Standard, FullFen("8/8/8/8/8/8/8/8 w - - 0 2"))
    assertRoundtrip(Standard, FullFen("8/8/8/8/8/8/8/8 b - - 0 2"))
    assertRoundtrip(Standard, FullFen("8/8/8/8/8/8/8/8 b - - 100 432"))
    assertRoundtrip(Standard, FullFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"))
    assertRoundtrip(Standard, FullFen("4nrk1/1pp3pp/p4p2/4P3/2BB1n2/8/PP3P1P/2K3R1 b - - 1 25"))
    assertRoundtrip(Standard, FullFen("5k2/6p1/8/1Pp5/6P1/8/8/3K4 w - c6 0 1"))
    assertRoundtrip(Standard, FullFen("4k3/8/8/8/3pP3/8/6N1/7K b - e3 0 1"))
    assertRoundtrip(Standard, FullFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"))
    assertRoundtrip(Standard, FullFen("r1k1r2q/p1ppp1pp/8/8/8/8/P1PPP1PP/R1K1R2Q w KQkq - 0 1"))
    assertRoundtrip(Standard, FullFen("r1k2r1q/p1ppp1pp/8/8/8/8/P1PPP1PP/R1K2R1Q w KQkq - 0 1"))
    assertRoundtrip(Standard, FullFen("8/8/8/4B2b/6nN/8/5P2/2R1K2k w Q - 1 1"))
    assertRoundtrip(Standard, FullFen("2r5/8/8/8/8/8/6PP/k2KR3 w K - 0 2"))
    assertRoundtrip(Standard, FullFen("4r3/3k4/8/8/8/8/6PP/qR1K1R2 w KQ - 2 1"))
    assertRoundtrip(Standard, FullFen("4rrk1/pbbp2p1/1ppnp3/3n1pqp/3N1PQP/1PPNP3/PBBP2P1/4RRK1 w Ff - 0 3"))
    assertRoundtrip(Standard, FullFen("8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3 3 1"))
    assertRoundtrip(Standard, FullFen("r2r3k/p7/3p4/8/8/P6P/8/R3K2R b KQq - 0 4"))

    assertRoundtrip(Chess960, FullFen("rn2k1r1/ppp1pp1p/3p2p1/5bn1/P7/2N2B2/1PPPPP2/2BNK1RR w Gkq - 4 11"))

    assertRoundtrip(FromPosition, FullFen("8/8/8/8/8/8/2Rk4/1K6 w - - 0 1"))

    assertRoundtrip(
      Horde,
      FullFen("rn1qkb1r/3bn1p1/2p3P1/pPP2P2/P1PPP1P1/P1PP1PPP/PPPPPPPP/PPPPPPPP w kq a6 0 12")
    )

    assertRoundtrip(Antichess, FullFen("8/2p1p2p/2Q1N2B/8/p7/N7/PPP1P1PP/R4B1R b - - 0 13"))
    assertRoundtrip(Antichess, FullFen("8/p6p/4p3/1P4P1/Pp4p1/3P4/7P/8 b - a3 0 1"))
    assertRoundtrip(Antichess, FullFen("8/p6p/4p3/1P4P1/1p4pP/3P4/P7/8 b - h3 0 1"))
    assertRoundtrip(Antichess, FullFen("8/7p/4p3/pP4P1/1p1P2p1/8/P6P/8 w - a6 0 2"))
    assertRoundtrip(Antichess, FullFen("8/p7/4p3/1P4Pp/1p1P2p1/8/P6P/8 w - h6 0 2"))

    assertRoundtrip(Atomic, FullFen("rnbq3r/ppp1p1pp/5p2/3p4/8/8/PPPPPPPP/RNBQKB1R b KQ - 0 4"))
    assertRoundtrip(Atomic, FullFen("8/6pp/2p2p1n/3p4/4P3/B6P/3P1PP1/1r2K2R b K - 0 17"))

    assertRoundtrip(RacingKings, FullFen("8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1"))
    assertRoundtrip(RacingKings, FullFen("8/8/8/8/8/6K1/krbnNBR1/qrbnNBRQ b - - 1 1"))

    assertRoundtrip(KingOfTheHill, FullFen("rnbq1bnr/ppp2ppp/3k4/4p2Q/3PK3/8/PPP2PPP/RNB2BNR b - - 0 7"))

    assertRoundtrip(
      ThreeCheck,
      FullFen("1r3rk1/pbp1N1pp/3p1q2/1p2bp2/7P/2PBB1P1/PP3Q1R/R5K1 b - - 3 21 +2+1")
    )

    assertRoundtrip(
      Crazyhouse,
      FullFen("1r3Q1n/p1kp3p/1p2ppq1/2p2b2/8/3P2P1/PPP1PPBP/R4RK1/NRpnnbb w - - 2 28")
    )
    assertRoundtrip(Crazyhouse, FullFen("b2nkbnQ~/p1pppp1p/pP1q2p1/r7/8/R5PR/P1PP1P1P/1NBQ1BNK/R w - - 1 2"))
    assertRoundtrip(Crazyhouse, FullFen("8/8/8/8/8/8/8/8/ w - - 0 1"))
    assertRoundtrip(
      Crazyhouse,
      FullFen("r~n~b~q~kb~n~r~/pppppppp/8/8/8/8/PPPPPPPP/RN~BQ~KB~NR/ w KQkq - 0 1")
    )

  test("fen fixtures roundtrip"):
    for fen <- FenFixtures.fens do assertRoundtrip(Standard, fen)

  test("binary format is stable"):
    assertPersistence(Standard, FullFen("8/8/8/8/8/8/8/8 w - - 0 1"), "0000000000000000")
    assertPersistence(Standard, FullFen("8/8/8/8/8/8/8/8 b - - 0 1"), "00000000000000000001")
    assertPersistence(Standard, FullFen("8/8/8/8/8/8/8/8 b - - 100 432"), "000000000000000064df06")
    assertPersistence(
      Standard,
      FullFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"),
      "ffff00001000efff2d844ad200000000111111113e955fe3"
    )
    assertPersistence(Standard, FullFen("5k2/6p1/8/1Pp5/6P1/8/8/3K4 w - c6 0 1"), "20400006400000080ac0b1")
    assertPersistence(Standard, FullFen("4k3/8/8/8/3pP3/8/6N1/7K b - e3 0 1"), "10000000180040802ac10f")
    assertPersistence(
      Chess960,
      FullFen("4rrk1/pbbp2p1/1ppnp3/3n1pqp/3N1PQP/1PPNP3/PBBP2P1/4RRK1 w Ff - 0 3"),
      "704f1ee8e81e4f70d60a44000002020813191113511571be000402"
    )
    assertPersistence(
      FromPosition,
      FullFen("8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3 3 1"),
      "00000002180000308a1c0f030103"
    )
    assertPersistence(
      Standard,
      FullFen("r2r3k/p7/3p4/8/8/P6P/8/R3K2R b KQq - 0 4"),
      "8901080000810091ad0d10e1f70007"
    )

    assertPersistence(
      KingOfTheHill,
      FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
      "ffff00000000ffff2d844ad200000000111111113e955be3000004"
    )
    assertPersistence(
      ThreeCheck,
      FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 99 1 +0+1"),
      "ffff00000000ffff2d844ad200000000111111113e955be363000501"
    )
    assertPersistence(Antichess, FullFen("8/7p/8/8/8/8/3K4/8 b - - 0 1"), "00800000000008001a000106")
    assertPersistence(
      Atomic,
      FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 2 3"),
      "ffff00000000ffff2d844ad200000000111111113e955be3020407"
    )
    assertPersistence(
      Horde,
      FullFen("rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1"),
      "ffff0066ffffffff000000000000000000000000000000000000111111113e955be3000008"
    )
    assertPersistence(
      RacingKings,
      FullFen("8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1"),
      "000000000000ffff793542867b3542a6000009"
    )
    assertPersistence(
      Crazyhouse,
      FullFen("r~n~b~q~kb~n~r~/pppppppp/8/8/8/8/PPPPPPPP/RN~BQ~KB~NR/ w KQkq - 0 499"),
      "ffff00000000ffff2d844ad200000000111111113e955be300e407010000000000ef0000000000002a"
    )
    assertPersistence(
      Crazyhouse,
      FullFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"),
      "ffff00000000ffff2d844ad200000000111111113e955be30000010000000000"
    )

  private def assertRoundtrip(variant: Variant, fen: FullFen) =
    val situation    = Fen.readWithMoveNumber(variant, fen).get
    val bytes        = BinaryFen.write(situation)
    val roundtripped = bytes.read
    assertEquals(Fen.write(roundtripped), fen)

  private def assertPersistence(variant: Variant, fen: FullFen, hex: String) =
    val situation = Fen.readWithMoveNumber(variant, fen).get
    val bytes     = BinaryFen.write(situation)
    assertEquals(bytes.value.map("%02x" format _).mkString, hex)
