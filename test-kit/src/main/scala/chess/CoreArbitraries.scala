package chess

import chess.format.pgn.{ Glyph, Glyphs }
import chess.format.{ Uci, UciCharPair }
import chess.variant.{ Crazyhouse, Variant }
import org.scalacheck.{ Arbitrary, Cogen, Gen }

object CoreArbitraries:
  given Arbitrary[Color] = Arbitrary(Gen.oneOf(Color.all))
  given Arbitrary[Side] = Arbitrary(Gen.oneOf(Side.all))
  given Arbitrary[Role] = Arbitrary(Gen.oneOf(Role.all))
  given Arbitrary[File] = Arbitrary(Gen.oneOf(File.all))
  given Arbitrary[Rank] = Arbitrary(Gen.oneOf(Rank.all))
  given Arbitrary[Square] = Arbitrary(Gen.oneOf(Square.all))
  given Arbitrary[Variant] = Arbitrary(Gen.oneOf(Variant.list.all))
  given Arbitrary[Glyph] = Arbitrary(Gen.oneOf(Glyphs.all))
  given Arbitrary[Glyphs] = Arbitrary:
    Gen.listOf(Arbitrary.arbitrary[Glyph]).map(Glyphs.fromList)
  given Arbitrary[Centis] = Arbitrary(Gen.posNum[Int].map(Centis(_)))

  given Arbitrary[Piece] = Arbitrary:
    for
      color <- Arbitrary.arbitrary[Color]
      role <- Arbitrary.arbitrary[Role]
    yield Piece(color, role)

  given Arbitrary[Castles] = Arbitrary(castlesGen)
  given Arbitrary[Bitboard] = Arbitrary(Gen.long.map(Bitboard(_)))

  given Arbitrary[PromotableRole] = Arbitrary(Gen.oneOf(Rook, Knight, Bishop, Queen))

  given Arbitrary[Uci] = Arbitrary(Gen.oneOf(normalUciMoveGen, promotionUciMoveGen, dropUciMoveGen))

  given Cogen[Color] = Cogen.cogenBoolean.contramap(_.white)
  given Cogen[Square] = Cogen(_.value.toLong)
  given Cogen[Centis] = Cogen(_.value.toLong)

  given [A](using Arbitrary[A]): Arbitrary[ByColor[A]] = Arbitrary:
    for
      w <- Arbitrary.arbitrary[A]
      b <- Arbitrary.arbitrary[A]
    yield ByColor(w, b)

  given Arbitrary[Crazyhouse.Pocket] = Arbitrary:
    for
      pawn <- Gen.oneOf(0 to 8)
      knight <- Gen.oneOf(0 to 2)
      bishop <- Gen.oneOf(0 to 2)
      rook <- Gen.oneOf(0 to 2)
      queen <- Gen.oneOf(0 to 2)
    yield Crazyhouse.Pocket(pawn, knight, bishop, rook, queen)

  given Arbitrary[Crazyhouse.Data] = Arbitrary:
    for
      pockets <- Arbitrary.arbitrary[ByColor[Crazyhouse.Pocket]]
      promoted <- Arbitrary.arbitrary[Bitboard]
    yield Crazyhouse.Data(pockets, promoted)

  def normalUciMoveGen =
    for
      orig <- Arbitrary.arbitrary[Square]
      dest <- Arbitrary.arbitrary[Square]
    yield Uci.Move(orig, dest)

  def promotionUciMoveGen =
    for
      file <- Arbitrary.arbitrary[File]
      rank <- Gen.oneOf(Rank.Second, Rank.Seventh)
      role <- Gen.oneOf(Role.allPromotable)
      offset <- Gen.oneOf(-1, 1)
      destFile = File(file.value + offset).getOrElse(file)
      orig = Square(file, rank)
      dest = Square(destFile, UciCharPair.implementation.lastRank(orig))
    yield Uci.Move(orig, dest, Some(role))

  def dropUciMoveGen =
    for
      dest <- Arbitrary.arbitrary[Square]
      role <- Gen.oneOf(Pawn, Knight, Bishop, Rook, Queen)
    yield Uci.Drop(role, dest)

  private val genBool = Gen.prob(0.5)
  private val castlesGen =
    for
      wks <- genBool
      wqs <- genBool
      bks <- genBool
      bqs <- genBool
    yield Castles(wks, wqs, bks, bqs)

  private def squareFromBitboardGen(bb: Bitboard): Gen[Square] =
    Gen.oneOf(bb.squares)

  private def corner(side: Side, color: Color): Square =
    side.fold(
      color.fold(Square.H1, Square.H8),
      color.fold(Square.A1, Square.A8)
    )

  def castleGen(color: Color): Gen[Move.Castle] =
    import File.*
    for
      side <- Arbitrary.arbitrary[Side]
      kingTo = Square(side.castledKingFile, color.backRank)
      rookTo = Square(side.castledRookFile, color.backRank)
      king <- squareFromBitboardGen(color.backRank.bb & (~kingTo.bb & ~H.bb & ~A.bb))
      cornerSquare = corner(side, color)
      rook <- squareFromBitboardGen(Bitboard.ray(king, cornerSquare))
    yield Move.Castle(king, kingTo, rook, rookTo)

  given Arbitrary[Move.Castle] = Arbitrary:
    Arbitrary.arbitrary[Color].flatMap(castleGen)
