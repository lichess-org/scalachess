package chess
package format.pgn

import scala.util.Try

object Binary {

  def writeMove(m: String) = Try(Writer move m)
  def writeMoves(ms: List[String]) = Try(Writer moves ms)

  def readMoves(bs: List[Byte]) = Try(Reader moves bs)

  private object MoveType {

    val SimplePawn = 0
    val SimplePiece = 1
    val FullPawn = 2
    val FullPiece = 3
  }

  private object Encoding {
    val pieceInts: Map[String, Int] = Map("K" -> 1, "Q" -> 2, "R" -> 3, "N" -> 4, "B" -> 5, "O-O" -> 6, "O-O-O" -> 7)
    val pieceStrs: Map[Int, String] = (pieceInts map { case (k, v) => v -> k }).toMap
    val promotionInts: Map[String, Int] = Map("" -> 0, "Q" -> 1, "R" -> 2, "N" -> 3, "B" -> 4)
    val promotionStrs: Map[Int, String] = (promotionInts map { case (k, v) => v -> k }).toMap
    val checkInts: Map[String, Int] = Map("" -> 0, "+" -> 1, "#" -> 2)
    val checkStrs: Map[Int, String] = (checkInts map { case (k, v) => v -> k }).toMap
  }

  private object Reader {

    import Encoding._

    def moves(bs: List[Byte]): List[String] = intMoves(bs map toInt)

    def intMoves(bs: List[Int]): List[String] = bs match {
      case Nil => Nil
      case b1 :: rest if moveType(b1) == MoveType.SimplePawn =>
        simplePawn(b1) :: intMoves(rest)
      case b1 :: b2 :: rest if moveType(b1) == MoveType.SimplePiece =>
        simplePiece(b1, b2) :: intMoves(rest)
      case b1 :: b2 :: rest if moveType(b1) == MoveType.FullPawn =>
        fullPawn(b1, b2) :: intMoves(rest)
      case b1 :: b2 :: b3 :: rest if moveType(b1) == MoveType.FullPiece =>
        fullPiece(b1, b2, b3) :: intMoves(rest)
      case x => !!(x map showByte mkString ",")
    }

    def simplePawn(i: Int): String = posString(right(i, 6))

    def simplePiece(b1: Int, b2: Int): String = pieceStrs(b2 >> 5) match {
      case castle@("O-O" | "O-O-O") => {
        val check = checkStrs(cut(b2, 5, 3))
        s"$castle$check"
      }
      case piece => {
        val pos = posString(right(b1, 6))
        val capture = if (bitAt(b2, 3)) "x" else ""
        val check = checkStrs(cut(b2, 5, 3))
        s"$piece$capture$pos$check"
      }
    }

    def fullPawn(b1: Int, b2: Int): String = {
      val pos = posString(right(b1, 6))
      val fileCapture = (b2 >> 6) match {
        case 0 => ""
        case 1 => (pos(0) - 1).toChar + "x"
        case 2 => (pos(0) + 1).toChar + "x"
      }
      val check = checkStrs(cut(b2, 6, 4))
      val prom = promotionStrs(cut(b2, 4, 1))
      val promotion = if (prom.isEmpty) "" else s"=$prom"
      s"$fileCapture$pos$promotion$check"
    }

    def fullPiece(b1: Int, b2: Int, b3: Int): String = {
      val pos = posString(right(b1, 6))
      val piece = pieceStrs(b2 >> 5)
      val capture = if (bitAt(b2, 3)) "x" else ""
      val check = checkStrs(cut(b2, 5, 3))
      val disamb = (b3 >> 6) match {
        case 0 => fileChar(right(b3, 3)).toString
        case 1 => rankChar(right(b3, 3)).toString
        case _ => posString(right(b3, 6))
      }
      s"$piece$disamb$capture$pos$check"
    }

    def moveType(i: Int) = i >> 6
    def posString(i: Int) = fileChar(i >> 3).toString + rankChar(right(i, 3))
    def fileChar(i: Int) = (i + 97).toChar
    def rankChar(i: Int) = (i + 49).toChar

    @inline private def right(i: Int, x: Int): Int = i & lengthMasks(x)
    @inline private def cut(i: Int, from: Int, to: Int): Int = right(i, from) >> to
    @inline private def bitAt(i: Int, p: Int): Boolean = cut(i, p, p - 1) != 0
    val bitMasks = Map(0 -> 0x80, 1 -> 0x40, 2 -> 0x20, 3 -> 0x10, 4 -> 0x08, 5 -> 0x04, 6 -> 0x02, 7 -> 0x01)
    val lengthMasks = Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0F, 5 -> 0x1F, 6 -> 0x3F, 7 -> 0x7F, 8 -> 0xFF)
    def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    import Encoding._

    def move(str: String): List[Byte] = (str match {
      case pos if pos.size == 2  => simplePawn(pos)
      case CastlingR(str, check) => castling(str, check)
      case SimplePieceR(piece, capture, pos, check) =>
        simplePiece(piece, pos, capture, check)
      case FullPawnR(file, pos, promotion, check) =>
        fullPawn(Option(file), pos, check, Option(promotion))
      case FullPieceR(piece, orig, capture, pos, check) =>
        fullPiece(piece, orig, pos, capture, check)
    }) map (_.toByte)

    def moves(strs: List[String]): List[Byte] = strs flatMap move

    def simplePawn(pos: String) = List(
      (MoveType.SimplePawn << 6) + posInt(pos)
    )

    def simplePiece(piece: String, pos: String, capture: String, check: String) = List(
      (MoveType.SimplePiece << 6) + posInt(pos),
      (pieceInts(piece) << 5) + (checkInts(check) << 3) + (boolInt(capture) << 2)
    )

    def castling(str: String, check: String) = List(
      MoveType.SimplePiece << 6,
      (pieceInts(str) << 5) + (checkInts(check) << 3)
    )

    def fullPawn(file: Option[String], pos: String, check: String, promotion: Option[String]) = List(
      (MoveType.FullPawn << 6) + posInt(pos),
      (shiftOptionInt(file, pos) << 6) + (checkInts(check) << 4) + (promotionInts(promotion | "") << 1)
    )

    def fullPiece(piece: String, orig: String, pos: String, capture: String, check: String) = List(
      (MoveType.FullPiece << 6) + posInt(pos),
      (pieceInts(piece) << 5) + (checkInts(check) << 3) + (boolInt(capture) << 2),
      (disambTypeInt(orig) << 6) + disambiguationInt(orig)
    )

    def disambTypeInt(orig: String): Int =
      if (orig.size > 1) 2
      else if (orig.head.toInt < 97) 1 else 0

    def disambiguationInt(orig: String): Int =
      if (orig.size > 1) posInt(orig)
      else if (orig.head.toInt < 97) rankInt(orig.head) else fileInt(orig.head)

    def boolInt(s: String): Int = if (s.nonEmpty) 1 else 0
    def boolInt(b: Boolean): Int = if (b) 1 else 0

    def posInt(pos: String): Int = posInt(fileInt(pos.head), rankInt(pos(1)))
    def posInt(x: Int, y: Int): Int = (x << 3) + y

    def fileInt(c: Char): Int = c.toInt - 97
    def rankInt(c: Char): Int = c.toInt - 49

    def shiftOptionInt(fileOption: Option[String], pos: String): Int =
      fileOption.fold(0) { file =>
        if (file.head < pos.head) 1 else 2
      }

    val pieceR = "([KQRNB])"
    val fileR = "(?:([a-h])x)?"
    val posR = "([a-h][1-9])"
    val captureR = "(x?)"
    val checkR = "([\\+#]?)"
    val promotionR = "(?:\\=?([QRNB]))?"
    val origR = "([a-h]?[1-8]?)".r
    val SimplePieceR = s"^$pieceR$captureR$posR$checkR$$".r
    val FullPawnR = s"^${fileR}$posR$promotionR$checkR$$".r
    val CastlingR = s"^(O-O|O-O-O)$checkR$$".r
    val FullPieceR = s"^$pieceR$origR$captureR$posR$checkR$$".r
  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String = "%08d" format (b.toBinaryString.toInt)
}
