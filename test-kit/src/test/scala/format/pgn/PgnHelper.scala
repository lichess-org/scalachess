package chess
package format.pgn

object PgnHelper:

  extension (pgn: ParsedPgn)
    def cleanTags: ParsedPgn =
      pgn.copy(tags = Tags.empty)
