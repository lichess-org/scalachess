package chess
package format.pgn

class TagTest extends ChessTest:

  // http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1
  test("be sorted"):
    assertEquals(
      Tags(
        List(
          Tag(Tag.Site, "https://lichess.org/QuCzSfxw"),
          Tag(Tag.Round, "-"),
          Tag(Tag.Date, "2018.05.04"),
          Tag(Tag.Black, "penguingim1"),
          Tag(Tag.White, "DrDrunkenstein"),
          Tag(Tag.Result, "1-0"),
          Tag(Tag.UTCDate, "2018.05.04"),
          Tag(Tag.UTCTime, "20:59:23"),
          Tag(Tag.WhiteElo, "2870"),
          Tag(Tag.BlackElo, "2862"),
          Tag(Tag.WhiteRatingDiff, "+12"),
          Tag(Tag.BlackRatingDiff, "-7"),
          Tag(Tag.Event, "Titled Arena 5")
        )
      ).sorted.value.map(_.name),
      List(
        Tag.Event,
        Tag.Site,
        Tag.Date,
        Tag.Round,
        Tag.White,
        Tag.Black,
        Tag.Result,
        Tag.UTCDate,
        Tag.UTCTime,
        Tag.WhiteElo,
        Tag.BlackElo,
        Tag.WhiteRatingDiff,
        Tag.BlackRatingDiff
      )
    )

  test("be trimmed"):
    assertEquals(
      List(
        Tag(_.Site, "  https://lichess.org/QuCzSfxw "),
        Tag(_.Black, " penguingim1  ")
      ),
      List(
        Tag(_.Site, "https://lichess.org/QuCzSfxw"),
        Tag(_.Black, "penguingim1")
      )
    )

  test("sanitize contents"):
    assertEquals(Tags.sanitize(List(Tag(Tag.TimeControl, "5+3"))).value, List(Tag(Tag.TimeControl, "5+3")))
    assertEquals(
      Tags.sanitize(List(Tag(Tag.TimeControl, "5400/40+30:1800+30\""))).value,
      List(Tag(Tag.TimeControl, "5400/40+30:1800+30"))
    )
    assertEquals(
      Tags.sanitize(List(Tag(Tag.White, "Schwarzenegger, Arnold \"The Terminator\""))).value,
      List(Tag(Tag.White, "Schwarzenegger, Arnold \"The Terminator\""))
    )
