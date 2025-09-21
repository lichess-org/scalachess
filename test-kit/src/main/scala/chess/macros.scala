package chess

import chess.format.Uci
import chess.format.pgn.*
import org.typelevel.literally.Literally

object macros:
  extension (inline ctx: StringContext)

    inline def pgn(inline args: Any*): ParsedPgn =
      ${ PgnLiteral('ctx, 'args) }

    inline def uci(inline args: Any*): Uci =
      ${ UciLiteral('ctx, 'args) }

  object PgnLiteral extends Literally[ParsedPgn]:
    def validate(s: String)(using Quotes) =
      Parser.full(PgnStr(s)) match
        case Right(_) => Right('{ Parser.full(PgnStr(${ Expr(s) })).toOption.get })
        case Left(err) => Left(err.toString)

  object UciLiteral extends Literally[Uci]:
    def validate(s: String)(using Quotes) =
      Uci(s) match
        case Some(_) => Right('{ Uci(${ Expr(s) }).get })
        case _ => Left(s"Invalid UCI: $s")
