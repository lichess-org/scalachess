package chess
package format

import chess.variant.Crazyhouse

/** https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
trait FenWriter:

  private given Ordering[File] = Ordering.by[File, Int](_.value)
  given Ordering[Square] = Ordering.by[Square, File](_.file)

  def write(position: Position): FullFen =
    write(position, FullMoveNumber(1))

  def write(parsed: Position.AndFullMoveNumber): FullFen =
    write(parsed.position, parsed.fullMoveNumber)

  def write(game: Game): FullFen =
    write(game.position, game.ply.fullMoveNumber)

  def write(position: Position, fullMoveNumber: FullMoveNumber): FullFen = FullFen:
    val builder = scala.collection.mutable.StringBuilder(80)
    builder.append(writeBoard(position))
    builder.append(writeCrazyPocket(position.crazyData))
    builder.addOne(' ')
    builder.addOne(position.color.letter)
    builder.addOne(' ')
    builder.append(writeCastles(position))
    builder.addOne(' ')
    builder.append(position.enPassantSquare.fold("-")(_.key))
    builder.addOne(' ')
    builder.append(position.history.halfMoveClock)
    builder.addOne(' ')
    builder.append(fullMoveNumber)
    if position.variant == variant.ThreeCheck then
      builder.addOne(' ')
      builder.append(writeCheckCount(position.history.checkCount))
    builder.toString

  def writeOpening(position: Position): StandardFen = StandardFen:
    s"${writeBoard(position)} ${position.color.letter} ${writeCastles(position)} ${position.enPassantSquare
        .fold("-")(_.key)}"

  def writeBoard(position: Position): BoardFen =
    val fen = scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for y <- Rank.allReversed do
      empty = 0
      for x <- File.all do
        position.pieceAt(x, y) match
          case None => empty = empty + 1
          case Some(piece) =>
            if empty == 0 then fen.append(piece.forsyth.toString)
            else
              fen.append(s"$empty${piece.forsyth}")
              empty = 0
            if piece.role != Pawn && position.crazyData.exists(_.promoted.contains(Square(x, y))) then
              fen.append('~')
      if empty > 0 then fen.append(empty)
      if y > Rank.First then fen.append('/')
    BoardFen(fen.toString)

  def writeBoardAndColor(position: Position): BoardAndColorFen =
    writeBoardAndColor(position, position.color)

  def writeBoardAndColor(position: Position, turnColor: Color): BoardAndColorFen =
    writeBoard(position).andColor(turnColor)

  private def writeCheckCount(checkCount: CheckCount) =
    s"+${checkCount.black}+${checkCount.white}"

  private def writeCrazyPocket(data: Option[Crazyhouse.Data]) =
    data match
      case Some(variant.Crazyhouse.Data(pockets, _)) =>
        "/" + pockets.forsyth
      case _ => ""

  private[chess] def writeCastles(position: Position): String =
    val cr = position.castlingRights.bb
    val wr = position.rooks & position.white & Bitboard.rank(White.backRank)
    val br = position.rooks & position.black & Bitboard.rank(Black.backRank)
    val wcr = cr & Bitboard.rank(White.backRank)
    val bcr = cr & Bitboard.rank(Black.backRank)
    val whiteKing = (position.white & position.kings & Bitboard.rank(White.backRank)).first
    val blackKing = (position.black & position.kings & Bitboard.rank(Black.backRank)).first

    // Emit castling notation for one color: standard K/Q if the rook is on the
    // outermost file of its side, otherwise x-FEN file letter.
    def perSide(
        rights: Bitboard,
        king: Option[Square],
        allRooks: Bitboard,
        kLetter: String,
        qLetter: String,
        fileChar: Square => String
    ): String =
      king.fold(""): k =>
        val kingSideStr = rights
          .findLast(_.file > k.file)
          .fold(""): sq =>
            if allRooks.last.contains(sq) then kLetter else fileChar(sq)
        val queenSideStr = rights
          .find(_.file < k.file)
          .fold(""): sq =>
            if allRooks.first.contains(sq) then qLetter else fileChar(sq)
        kingSideStr + queenSideStr

    val result =
      perSide(wcr, whiteKing, wr, "K", "Q", _.file.toUpperCaseString) +
        perSide(bcr, blackKing, br, "k", "q", _.file.char.toString)
    if result.isEmpty then "-" else result
