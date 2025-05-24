package chess

import cats.syntax.all.*
import chess.format.pgn.Sans.*
import chess.format.pgn.{ Parser, Reader, San, SanStr }
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

  def gameMoveWhileValidReverse(
      sans: Seq[SanStr],
      initialFen: Fen.Full,
      variant: Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[ErrorStr]) =
    inline def transform(success: (next: Game, move: MoveOrDrop)) =
      (success.next, Uci.WithSan(success.move.toUci, success.move.toSanStr))
    Game(variant.some, initialFen.some).playWhileValidReverse(sans, Ply.initial)(transform)

  def gameMoveWhileValid(
      sans: Seq[SanStr],
      initialFen: Fen.Full,
      variant: Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[ErrorStr]) =
    gameMoveWhileValidReverse(sans, initialFen, variant) match
      case (game, gs, err) => (game, gs.reverse, err)

  private def computeReplay(replay: Replay, moves: List[Moveable]): Either[ErrorStr, Replay] =
    moves.foldM(replay) { (replay, uci) => uci(replay.state.position).map(replay.addMove(_)) }

  def apply(
      moves: List[Uci],
      initialFen: Option[Fen.Full],
      variant: Variant
  ): Either[ErrorStr, Replay] =
    computeReplay(Replay(Game(variant.some, initialFen)), moves)

  def apply(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Full],
      variant: Variant
  ): Either[ErrorStr, Reader.Result] =
    if sans.isEmpty then ErrorStr("[replay] pgn is empty").asLeft
    else Reader.moves(sans, Game(variant.some, initialFen))

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
