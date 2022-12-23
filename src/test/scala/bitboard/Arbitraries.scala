package chess
package bitboard

import org.scalacheck.{ Arbitrary, Gen }

object Arbitraries:

  given Arbitrary[Pos] = Arbitrary(Gen.choose(0, 63).map(n => Pos.at(n).get))
  given Arbitrary[Bitboard] = Arbitrary(Gen.choose(0, 63).map(n => Bitboard(n)))

  given Arbitrary[File] = Arbitrary(Gen.choose(0, 7).map(n => File.atIndex(n).get))
  given Arbitrary[Rank] = Arbitrary(Gen.choose(0, 7).map(n => Rank.atIndex(n).get))
