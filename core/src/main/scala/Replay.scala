package chess

import cats.syntax.all.*
import chess.format.pgn.Sans.*
import chess.format.pgn.{ Parser, Reader, San, SanStr, Tag, Tags }
import chess.format.{ Fen, Uci }
import chess.variant.Variant

case class Replay(setup: Game, moves: List[MoveOrDrop], state: Game):

  lazy val chronoMoves: List[MoveOrDrop] = moves.reverse

  def addMove(moveOrDrop: MoveOrDrop): Replay =
    copy(
      moves = moveOrDrop :: moves,
      state = moveOrDrop.applyGame(state)
    )

  def moveAtPly(ply: Ply): Option[MoveOrDrop] =
    chronoMoves.lift(ply.value - 1 - setup.startedAtPly.value)

object Replay:

  def apply(game: Game): Replay = Replay(game, Nil, game)

  def apply(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Full],
      variant: Variant
  ): Either[ErrorStr, Reader.Result] =
    if sans.isEmpty then ErrorStr("[replay] pgn is empty").asLeft
    else
      Reader.moves(
        sans,
        Tags(
          List(
            initialFen.map(fen => Tag(_.FEN, fen.value)),
            variant.some.filterNot(_.standard).map(v => Tag(_.Variant, v.name))
          ).flatten
        )
      )

  def gameMoveWhileValidReverse(
      sans: Seq[SanStr],
      initialFen: Fen.Full,
      variant: Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[ErrorStr]) =
    val init: Game = makeGame(variant, initialFen.some)
    inline def transform(success: (next: Game, move: MoveOrDrop)) =
      (success.next, Uci.WithSan(success.move.toUci, success.move.toSanStr))
    Parser
      .moves(sans)
      .fold(
        err => (init, List.empty[(Game, Uci.WithSan)], err.some),
        moves => init.playMovesReverse(moves.value, init.ply, transform)
      )

  def gameMoveWhileValid(
      sans: Seq[SanStr],
      initialFen: Fen.Full,
      variant: Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[ErrorStr]) =
    gameMoveWhileValidReverse(sans, initialFen, variant) match
      case (game, gs, err) => (game, gs.reverse, err)

  @scala.annotation.tailrec
  private def computeReplay(replay: Replay, ucis: List[Uci]): Either[ErrorStr, Replay] =
    ucis match
      case Nil => replay.asRight
      case uci :: rest =>
        uci(replay.state.position) match
          case Left(err) => err.asLeft
          case Right(md) => computeReplay(replay.addMove(md), rest)

  private def makePosition(initialFen: Option[Fen.Full], variant: Variant): Position =
    (initialFen.flatMap(Fen.read) | Position(variant)).withVariant(variant)

  def boards(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Full],
      variant: Variant
  ): Either[ErrorStr, List[Position]] =
    Parser
      .moves(sans)
      .flatMap(moves => boardsFromUci(moves.value, initialFen, variant))

  def boardsFromUci[M <: Moveable](
      moves: List[Moveable],
      initialFen: Option[Fen.Full],
      variant: Variant
  ): Either[ErrorStr, List[Position]] =
    val init = makePosition(initialFen, variant)
    init.playPositions(moves).map(init :: _)

  def apply(
      moves: List[Uci],
      initialFen: Option[Fen.Full],
      variant: Variant
  ): Either[ErrorStr, Replay] =
    computeReplay(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Full],
      variant: Variant,
      atFen: Fen.Full
  ): Either[ErrorStr, Ply] =
    if Fen.read(variant, atFen).isEmpty then ErrorStr(s"Invalid Fen $atFen").asLeft
    else
      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: Fen.Full) = fen.value.split(' ').take(4).mkString(" ")
      val atFenTruncated             = truncateFen(atFen)
      def compareFen(fen: Fen.Full)  = truncateFen(fen) == atFenTruncated

      @scala.annotation.tailrec
      def recursivePlyAtFen(position: Position, sans: List[San], ply: Ply): Either[ErrorStr, Ply] =
        sans match
          case Nil => ErrorStr(s"Can't find $atFenTruncated, reached ply $ply").asLeft
          case san :: rest =>
            san(position) match
              case Left(err) => err.asLeft
              case Right(moveOrDrop) =>
                val after = moveOrDrop.after
                val fen   = Fen.write(after.withColor(ply.turn), ply.fullMoveNumber)
                if compareFen(fen) then ply.asRight
                else recursivePlyAtFen(after.withColor(!position.color), rest, ply.next)

      val position = initialFen.flatMap(Fen.read(variant, _)) | Position(variant)

      Parser
        .moves(sans)
        .flatMap(moves => recursivePlyAtFen(position, moves.value, Ply.firstMove))

  private def makeGame(variant: Variant, initialFen: Option[Fen.Full]): Game =
    val g = Game(variant.some, initialFen)
    g.copy(startedAtPly = g.ply)
