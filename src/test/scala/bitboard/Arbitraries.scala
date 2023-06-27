package chess
package bitboard

import org.scalacheck.{ Arbitrary, Cogen, Gen }

object Arbitraries:

  given Arbitrary[File]   = Arbitrary(Gen.oneOf(File.all))
  given Arbitrary[Rank]   = Arbitrary(Gen.oneOf(Rank.all))
  given Arbitrary[Square] = Arbitrary(Gen.oneOf(Square.all))

  given Arbitrary[Bitboard] = Arbitrary(Gen.long.map(Bitboard(_)))

  given Cogen[Square] = Cogen(_.value.toLong)
