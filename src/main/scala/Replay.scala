package chess

import cats.syntax.all.*

import chess.format.pgn.{ Parser, Reader, San, SanStr, Tag, Tags }
import chess.format.pgn.Sans.*
import chess.format.{ Fen, Uci }
import chess.variant.Variant
import MoveOrDrop.*

case class Replay(setup: Game, moves: List[MoveOrDrop], state: Game):

  lazy val chronoMoves: List[MoveOrDrop] = moves.reverse

  def addMove(moveOrDrop: MoveOrDrop) =
    copy(
      moves = moveOrDrop.applyVariantEffect :: moves,
      state = moveOrDrop.applyGame(state)
    )

  def moveAtPly(ply: Ply): Option[MoveOrDrop] =
    chronoMoves.lift(ply.value - 1 - setup.startedAtPly.value)

object Replay:

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Either[ErrorStr, Reader.Result] =
    if sans.isEmpty then ErrorStr("[replay] pgn is empty").asLeft
    else
      Reader.moves(
        sans,
        Tags(
          List(
            initialFen map { fen =>
              Tag(_.FEN, fen.value)
            },
            variant.some.filterNot(_.standard) map { v =>
              Tag(_.Variant, v.name)
            }
          ).flatten
        )
      )

  def gameMoveWhileValidReverse(
      sans: Seq[SanStr],
      initialFen: Fen.Epd,
      variant: Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[ErrorStr]) =
    val init       = makeGame(variant, initialFen.some)
    val emptyGames = List.empty[(Game, Uci.WithSan)]
    sans
      .foldM((init, emptyGames)):
        case ((head, games), str) =>
          Parser
            .sanOnly(str)
            .flatMap: san =>
              san(head.situation)
                .map: move =>
                  val newGame = move.applyGame(head)
                  (newGame, (newGame, Uci.WithSan(move.toUci, str)) :: games)
            .leftMap(err => (init, games, err.some))
      .map(gs => (init, gs._2, none))
      .merge

  def gameMoveWhileValid(
      sans: Seq[SanStr],
      initialFen: Fen.Epd,
      variant: Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[ErrorStr]) =
    gameMoveWhileValidReverse(sans, initialFen, variant) match
      case (game, gs, err) => (game, gs.reverse, err)

  private def computeSituations[M](
      sit: Situation,
      moves: List[M],
      play: M => Situation => Either[ErrorStr, MoveOrDrop]
  ): Either[ErrorStr, List[Situation]] =
    moves
      .foldM(List(sit)): (sits, move) =>
        val current = sits.head
        play(move)(current).map(md => Situation(md.finalizeAfter, !current.color) :: sits)
      .map(_.reverse)

  @scala.annotation.tailrec
  private def computeReplay(replay: Replay, ucis: List[Uci]): Either[ErrorStr, Replay] =
    ucis match
      case Nil => replay.asRight
      case uci :: rest =>
        uci(replay.state.situation) match
          case Left(err) => err.asLeft
          case Right(md) => computeReplay(replay.addMove(md), rest)

  private def initialFenToSituation(
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Situation = {
    initialFen.flatMap(Fen.read) | Situation(variant)
  } withVariant variant

  def boards(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Either[ErrorStr, List[Board]] =
    situations(sans, initialFen, variant) map (_ map (_.board))

  def situations(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Either[ErrorStr, List[Situation]] =
    val sit = initialFenToSituation(initialFen, variant)
    Parser
      .moves(sans)
      .flatMap: moves =>
        computeSituations(sit, moves.value, _.apply)

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Either[ErrorStr, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Either[ErrorStr, List[Situation]] =
    computeSituations(initialFenToSituation(initialFen, variant), moves, _.apply)

  def apply(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Either[ErrorStr, Replay] =
    computeReplay(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant,
      atFen: Fen.Epd
  ): Either[ErrorStr, Ply] =
    if Fen.read(variant, atFen).isEmpty then ErrorStr(s"Invalid Fen $atFen").asLeft
    else
      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: Fen.Epd) = fen.value.split(' ').take(4) mkString " "
      val atFenTruncated            = truncateFen(atFen)
      def compareFen(fen: Fen.Epd)  = truncateFen(fen) == atFenTruncated

      @scala.annotation.tailrec
      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Ply): Either[ErrorStr, Ply] =
        sans match
          case Nil => ErrorStr(s"Can't find $atFenTruncated, reached ply $ply").asLeft
          case san :: rest =>
            san(sit) match
              case Left(err) => err.asLeft
              case Right(moveOrDrop) =>
                val after = moveOrDrop.finalizeAfter
                val fen   = Fen write Game(Situation(after, ply.turn), ply = ply)
                if compareFen(fen) then ply.asRight
                else recursivePlyAtFen(Situation(after, !sit.color), rest, ply + 1)

      val sit = initialFen.flatMap { Fen.read(variant, _) } | Situation(variant)

      Parser
        .moves(sans)
        .flatMap: moves =>
          recursivePlyAtFen(sit, moves.value, Ply(1))

  private def makeGame(variant: Variant, initialFen: Option[Fen.Epd]): Game =
    val g = Game(variant.some, initialFen)
    g.copy(startedAtPly = g.ply)
