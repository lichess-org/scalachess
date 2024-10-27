package chess
package format.pgn

import cats.syntax.all.*
import org.typelevel.literally.Literally

object macros:
  extension (inline ctx: StringContext)
    inline def pgn(inline args: Any*): ParsedPgn =
      ${ PgnLiteral('ctx, 'args) }

  object PgnLiteral extends Literally[ParsedPgn]:
    def validate(s: String)(using Quotes) =
      Parser.full(PgnStr(s)) match
        case Right(parsed) => Right('{ Parser.full(PgnStr(${ Expr(s) })).toOption.get })
        case Left(err)     => Left(err.toString)
