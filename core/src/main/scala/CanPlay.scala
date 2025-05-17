package chess

import cats.syntax.all.*
import cats.{ Foldable, Traverse }
import chess.CanPlay.makeError
import chess.format.pgn.{ Parser, SanStr }
import chess.variant.Variant

import scala.annotation.targetName

trait HasPosition[A]:
  extension (a: A)
    def position: Position
    inline def variant: Variant = position.variant
    inline def color: Color     = position.color
    inline def history: History = position.history

trait CanPlay[A]:
  type Step      = (next: A, move: MoveOrDrop)
  type Result[B] = (state: A, moves: List[B], error: Option[ErrorStr])
  extension (a: A)
    /**
     * Play a move and return the new state and the move or drop that was played.
     */
    def apply[M <: Moveable](move: M): Either[ErrorStr, Step]

    def play[M <: Moveable, F[_]: Traverse](moves: F[M]): Either[ErrorStr, List[MoveOrDrop]] =
      play(moves, Ply.initial, _.move)

    def playSoft[M <: Moveable, F[_]: Traverse](moves: F[M]): Result[MoveOrDrop] =
      playSoft(moves, Ply.initial, _.move)

    def play[M <: Moveable, B, F[_]: Traverse](
        moves: F[M],
        initialPly: Ply,
        transform: Step => B
    ): Either[ErrorStr, List[B]] =
      val (moves = acc, error = error) = playReverse(moves, initialPly, transform)
      error.fold(acc.reverse.asRight)(_.asLeft)

    def playSoft[B, F[_]: Traverse](
        moves: F[SanStr],
        initialPly: Ply,
        transform: Step => B
    ): Either[ErrorStr, Result[B]] =
      Parser.moves(moves).map(playSoft(_, initialPly, transform))

    def playSoft[M <: Moveable, B, F[_]: Traverse](
        moves: F[M],
        initialPly: Ply,
        transform: Step => B
    ): Result[B] =
      val (state, acc, error) = playReverse(moves, initialPly, transform)
      (state = state, moves = acc.reverse, error = error)

    def playReverse[M <: Moveable, F[_]: Traverse](moves: F[M]): Result[MoveOrDrop] =
      playReverse(moves, Ply.initial, _.move)

    def playReverse[M <: Moveable, B, F[_]: Traverse](
        moves: F[M],
        initialPly: Ply,
        transform: Step => B
    ): (state: A, moves: List[B], error: Option[ErrorStr]) =
      moves.zipWithIndex
        .foldM((a, List.empty[B])) { case ((next, acc), (move, index)) =>
          next(move)
            .bimap(
              _ => (next, acc, makeError(initialPly + index, move).some),
              step => (state = step.next, moves = transform(step) :: acc)
            )
        }
        .fold(identity, (next, acc) => (state = next, moves = acc, error = none))

    def playPositions[M <: Moveable, F[_]: Traverse](moves: F[M])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Position]] =
      val result = playSoft(moves, Ply.initial, _.move.after)
      result.error.fold((a.position :: result.moves).asRight)(_.asLeft)

    @targetName("playPositionsFromSans")
    def playPositions[F[_]: Traverse](moves: F[SanStr])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Position]] =
      Parser.moves(moves).flatMap(playPositions)

    @targetName("playBoardsFromSans")
    def playBoards[F[_]: Traverse](moves: F[SanStr])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Board]] =
      Parser.moves(moves).flatMap(playBoards)

    def playBoards[M <: Moveable, F[_]: Traverse](moves: F[M])(using
        HasPosition[A]
    ): Either[ErrorStr, List[Board]] =
      val result = playSoft(moves, Ply.initial, _.move.after.board)
      result.error.fold((a.position.board :: result.moves).asRight)(_.asLeft)

    def validate[M <: Moveable, F[_]: Foldable](moves: F[M]): Either[ErrorStr, Unit] =
      moves.foldM(())((_, move) => a(move).void)

    def rewind[F[_]: Traverse](moves: F[SanStr]): Either[ErrorStr, A] =
      Parser.moves(moves).flatMap(rewind)

    def rewind[M <: Moveable, F[_]: Foldable](moves: F[M]): Either[ErrorStr, A] =
      moves.foldM(a)((state, move) => state(move).map(_.next))

    def movesWhileValidReverse[B, F[_]: Traverse](
        sans: F[SanStr],
        transform: Step => B
    ): (A, List[B], Option[ErrorStr]) =
      Parser
        .moves(sans)
        .fold(
          err => (a, List.empty[B], err.some),
          moves => a.playReverse(moves, Ply.initial, transform)
        )

    def movesWhileValid[B, F[_]: Traverse](
        sans: F[SanStr],
        transform: Step => B
    ): (A, List[B], Option[ErrorStr]) =
      val (state, moves, error) = movesWhileValidReverse(sans, transform)
      (state, moves.reverse, error)

    def movesWhileValid[F[_]: Traverse](
        sans: F[SanStr]
    ): (List[A], Option[ErrorStr]) =
      val (moves, error) = movesWhileValidReverse(sans)
      (moves.reverse, error)

    def movesWhileValidReverse[F[_]: Traverse](
        sans: F[SanStr]
    ): (List[A], Option[ErrorStr]) =
      Parser
        .moves(sans)
        .fold(
          err => (List.empty[A], err.some),
          moves =>
            val (_, xs, error) = a.playReverse(moves, Ply.initial, _.next)
            (xs :+ a, error)
        )

object CanPlay:

  inline def makeError(currentPly: Ply, move: Moveable): ErrorStr =
    val moveAt    = currentPly.fullMoveNumber.value
    val rawString = move.rawString.getOrElse(move.toString())
    ErrorStr(s"Cannot play $rawString at move $moveAt by ${currentPly.turn.name}")
