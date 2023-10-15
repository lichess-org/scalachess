package chess
package format.pgn

import ornicar.scalalib.time.*

class TimeFormatTest extends ChessSpecs:

  "format seconds" should:
    "look good" in:
      Move.formatPgnSeconds(0) === "0:00:00"
      Move.formatPgnSeconds(9) === "0:00:09"
      Move.formatPgnSeconds(60) === "0:01:00"
      Move.formatPgnSeconds(79835) === "22:10:35"
      Move.formatPgnSeconds(979835) === "272:10:35"

  "format PGN tags" should:
    "UTCDate" in:
      Tag.UTCDate.format.format(millisToDateTime(1680424483730L)) === "2023.04.02"
    "UTCDate" in:
      Tag.UTCTime.format.format(millisToDateTime(1680424483730L)) === "08:34:43"
