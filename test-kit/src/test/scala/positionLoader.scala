package chess

import java.nio.file.{ Files, Paths, Path }
import scala.io.{ Source, Codec }
import scala.util.Try
import scala.util.boundary
import scala.util.boundary.break

object FortressesCsv:

  final case class ParsedCase(
      id: String,
      fen: String,
      color: Color,
      shouldBreakthrough: Boolean,
      reachables: List[String]
  )

  private val squareRe = "^[a-h][1-8]$".r
  private val rangeRe = "^([a-h][1-8])\\s*-\\s*([a-h][1-8])$".r

  private def splitCsvLine(line: String): List[String] =
    val buf = new StringBuilder
    val out = collection.mutable.ListBuffer.empty[String]
    var i = 0
    var inQuotes = false
    while i < line.length do
      val ch = line.charAt(i)
      if inQuotes then
        if ch == '"' then
          val nextIsQuote = i + 1 < line.length && line.charAt(i + 1) == '"'
          if nextIsQuote then
            buf += '"'; i += 1
          else inQuotes = false
        else buf += ch
      else
        ch match
          case ',' => out += buf.result().trim; buf.clear()
          case '"' => inQuotes = true
          case _ => buf += ch
      i += 1
    out += buf.result().trim
    out.toList

  private def unquote(s: String): String =
    if s.startsWith("\"") && s.endsWith("\"") && s.length >= 2 then
      s.substring(1, s.length - 1).replace("\"\"", "\"").trim
    else s.trim

  private def parseColor(s: String): Either[String, Color] =
    s.trim.toLowerCase match
      case "white" => Right(Color.white)
      case "black" => Right(Color.black)
      case other => Left(s"Unknown color: $other")

  private def parseBool(s: String): Either[String, Boolean] =
    s.trim.toLowerCase match
      case "true" => Right(true)
      case "false" => Right(false)
      case other => Left(s"Expected true/false, got: $other")

  private def expandRange(tok: String): Either[String, List[String]] =
    tok.trim match
      case rangeRe(from, to) =>
        val (fFile, fRank) = (from.charAt(0), from.charAt(1))
        val (tFile, tRank) = (to.charAt(0), to.charAt(1))
        if fRank == tRank then
          val lo = math.min(fFile, tFile)
          val hi = math.max(fFile, tFile)
          Right((lo to hi).map(c => s"${c.toChar}$fRank").toList)
        else if fFile == tFile then
          val lo = math.min(fRank, tRank)
          val hi = math.max(fRank, tRank)
          Right((lo to hi).map(r => s"$fFile${r.toChar}").toList)
        else Left(s"Diagonal/rectangle ranges are not supported: $tok")
      case _ =>
        Left(s"Invalid range syntax: $tok")

  private def parseReachables(text: String): Either[String, List[String]] =
    val tokens =
      text.split("[,\\s]+").iterator.map(_.trim).filter(_.nonEmpty).toList
    boundary:
      val out = List.newBuilder[String]
      for tok <- tokens do
        tok match
          case squareRe() =>
            out += tok
          case _ =>
            expandRange(tok) match
              case Right(xs) => out ++= xs
              case Left(err) => break(Left(err))
      Right(out.result())

  private def resolveResourcesPath(filename: String): Either[String, Path] =
    val projectRoot = Paths.get("").toAbsolutePath.normalize
    val target = projectRoot.resolve("test-kit/src/test/resources").resolve(filename)
    if Files.exists(target) then Right(target)
    else Left(s"Could not find ${target.toString}")

  def load(filename: String): Either[String, List[ParsedCase]] =
    for
      path <- resolveResourcesPath(filename)
      lines <- Try(Source.fromFile(path.toFile)(using Codec.UTF8).getLines().toList).toEither.left
        .map(_.getMessage)
      rows = lines.filter(l => l.nonEmpty && !l.trim.startsWith("#"))
      parsed <- parseRows(rows)
    yield parsed

  private def parseRows(rows: List[String]): Either[String, List[ParsedCase]] =
    rows match
      case Nil => Right(Nil)
      case header :: rest =>
        val cols = splitCsvLine(header).map(_.toLowerCase)
        val expected = List("id", "fen", "color", "shouldbreakthrough", "reachables")
        if cols != expected then
          Left(s"CSV header mismatch. Expected: ${expected.mkString(",")} but got: ${cols.mkString(",")}")
        else
          boundary:
            val out = List.newBuilder[ParsedCase]
            var lineNo = 1
            for line <- rest do
              lineNo += 1
              val fields = splitCsvLine(line).map(unquote)
              if fields.length != 5 then break(Left(s"Line $lineNo: expected 5 fields, got ${fields.length}"))
              val id = fields(0)
              val fen = fields(1)
              val colorS = fields(2)
              val sbS = fields(3)
              val reach = fields(4)

              (for
                color <- parseColor(colorS)
                sb <- parseBool(sbS)
                rs <- parseReachables(reach)
              yield ParsedCase(id, fen, color, sb, rs)) match
                case Left(err) => break(Left(s"Line $lineNo ($id): $err"))
                case Right(pc) => out += pc
            Right(out.result())
