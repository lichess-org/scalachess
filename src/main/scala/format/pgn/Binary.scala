package chess
package format.pgn

object Binary {

  def writeMove(m: String) = Writer move m
  def writeMoves(ms: String) = Writer moves ms

  private object Writer {

    def move(str: String): List[Byte] = (str match {
      case pos if pos.size == 2 ⇒ MoveType simplePawn pos
      case CastlingR(str, check) ⇒ MoveType.castling(str, check)
      case SimplePieceR(piece, capture, pos, check) ⇒
        MoveType.simplePiece(piece, pos, capture, check)
      case FullPawnR(file, pos, promotion, check) ⇒
        MoveType.fullPawn(Option(file), pos, check, Option(promotion))
      case FullPieceR(piece, orig, capture, pos, check) =>
        MoveType.fullPiece(piece, orig, pos, capture, check)
    }) map (_.toByte)

    def moves(str: String): List[Byte] = str.split(' ').toList flatMap move

    object MoveType {

      val SimplePawn = 0
      val SimplePiece = 1
      val FullPawn = 2
      val FullPiece = 3

      def simplePawn(pos: String) = List(
        (SimplePawn << 6) + posInt(pos)
      )

      def simplePiece(piece: String, pos: String, capture: String, check: String) = List(
        (SimplePiece << 6) + posInt(pos),
        (pieceInt(piece) << 5) + (checkInt(check) << 3) + (boolInt(capture) << 2)
      )

      def castling(str: String, check: String) = List(
        SimplePiece << 6,
        (pieceInt(str) << 5) + (checkInt(check) << 3)
      )

      def fullPawn(file: Option[String], pos: String, check: String, promotion: Option[String]) = List(
        (FullPawn << 6) + posInt(pos),
        (shiftOptionInt(file, pos) << 6) + (checkInt(check) << 4) + (promotionInt(promotion) << 1)
      )

      def fullPiece(piece: String, orig: String, pos: String, capture: String, check: String) = List(
        (FullPiece << 6) + posInt(pos),
        (pieceInt(piece) << 5) + (checkInt(check) << 3) + (boolInt(capture) << 2),
        (disambTypeInt(orig) << 7) + (disambiguationInt(orig) << 1)
      )
    }

    def disambTypeInt(orig: String): Int = if (orig.size > 1) 1 else 0

    def disambiguationInt(orig: String): Int = 
      if (orig.size > 1) posInt(orig) else (fileInt(orig) << 3)

    def checkInt(s: String) = s match {
      case ""  ⇒ 0
      case "+" ⇒ 1
      case "#" ⇒ 2
    }

    def boolInt(s: String): Int = if (s.nonEmpty) 1 else 0
    def boolInt(b: Boolean): Int = if (b) 1 else 0

    def posInt(pos: String): Int = posInt(fileInt(pos), pos(1).toInt - 49)
    def posInt(x: Int, y: Int): Int = (x << 3) + y

    def fileInt(pos: String): Int = pos.head.toInt - 97

    def shiftOptionInt(fileOption: Option[String], pos: String): Int =
      fileOption.fold(0) { file ⇒
        if (file.head < pos.head) 1 else 2
      }

    val pieceInts = Map("K" -> 1, "Q" -> 2, "R" -> 3, "N" -> 4, "B" -> 5, "O-O" -> 6, "O-O-O" -> 7)
    def pieceInt(piece: String): Int = pieceInts(piece)

    val promotionInts = Map("Q" -> 1, "R" -> 2, "N" -> 3, "B" -> 4)
    def promotionInt(prom: Option[String]): Int = prom.fold(0)(promotionInts)

    val pieceR = "([KQRNB])"
    val fileR = "(?:([a-h])x)?"
    val posR = "([a-h][1-9])"
    val captureR = "(x?)"
    val checkR = "([\\+#]?)"
    val promotionR = "(?:\\=([QRNB]))?"
    val origR = "([a-h][1-8]|[a-h]|)".r
    val SimplePieceR = s"^$pieceR$captureR$posR$checkR$$".r
    val FullPawnR = s"^${fileR}$posR$promotionR$checkR$$".r
    val CastlingR = s"^(O-O|O-O-O)$checkR$$".r
    val FullPieceR = s"^$pieceR$origR$captureR$posR$checkR$$".r
  }

  def read(bin: List[Byte]): String = ""
}
