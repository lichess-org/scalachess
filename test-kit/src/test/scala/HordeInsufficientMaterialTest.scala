package chess

import cats.effect.IO
import cats.kernel.Monoid
import cats.syntax.all.*
import chess.format.{ Fen, FullFen }
import chess.variant.*
import fs2.*
import fs2.io.file.Files
import weaver.*

object HordeInsufficientMaterialTest extends SimpleIOSuite:

  test("horde"):
    run("test-kit/src/test/resources/horde_insufficient_material.csv", Horde).map(expect(_))

  given Monoid[Boolean] with
    def empty = true
    def combine(x: Boolean, y: Boolean) = x && y

  private def run(file: String, variant: Variant): IO[Boolean] =
    parser(file)
      .foldMap(_.run(variant))
      .compile
      .lastOrError

  private def parser(file: String): Stream[IO, Case] =
    Files[IO]
      .readAll(fs2.io.file.Path(file))
      .through(csvParser)
      .map(parseSample)

  private def csvParser[F[_]]: Pipe[F, Byte, List[String]] =
    _.through(text.utf8Decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.split(',').toList)

  private def parseSample(sample: List[String]): Case =
    Case(FullFen(sample(0)), sample(1).toBoolean, sample.get(2))

private case class Case(fen: FullFen, expected: Boolean, comment: Option[String]):
  def run(variant: Variant): Boolean =
    val board = Fen.read(variant, fen).get
    Horde.hasInsufficientMaterial(board.board, !board.color) == expected
