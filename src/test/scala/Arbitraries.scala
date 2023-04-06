package chess

import org.scalacheck.{ Arbitrary, Gen }

object Arbitraries:

  given Arbitrary[Castles] = Arbitrary(castlesGen)
  given Arbitrary[Color]   = Arbitrary(Gen.oneOf(Color.all))
  given Arbitrary[Side]    = Arbitrary(Gen.oneOf(Side.all))
  given Arbitrary[Role]    = Arbitrary(Gen.oneOf(Role.all))

  given Arbitrary[Piece] = Arbitrary(
    for
      color <- Arbitrary.arbitrary[Color]
      role  <- Arbitrary.arbitrary[Role]
    yield Piece(color, role)
  )

  private val genBool = Gen.prob(0.5)
  private val castlesGen =
    for
      wks <- genBool
      wqs <- genBool
      bks <- genBool
      bqs <- genBool
    yield Castles(wks, wqs, bks, bqs)
