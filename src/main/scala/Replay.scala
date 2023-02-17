package chess

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.syntax.all.*

import chess.format.pgn.{ Parser, Reader, San, SanStr, Tag, Tags }
import chess.format.{ Fen, Uci }
import chess.variant.Variant
import MoveOrDrop.*

case class Replay(setup: Game, moves: List[MoveOrDrop], state: Game):

  lazy val chronoMoves: List[MoveOrDrop] = moves.reverse

  def addMove(moveOrDrop: MoveOrDrop) =
    copy(
      moves = moveOrDrop.applyVariantEffect :: moves,
      state = moveOrDrop.fold(state.apply, state.applyDrop)
    )

  def moveAtPly(ply: Ply): Option[MoveOrDrop] =
    chronoMoves.lift(ply.value - 1 - setup.startedAtPly.value)

object Replay:

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[ErrorStr, Reader.Result] =
    sans.some.filter(_.nonEmpty).toValid(ErrorStr("[replay] pgn is empty")).andThen { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves,
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
    }

  private def computeGames(game: Game, sans: List[San]): Validated[ErrorStr, List[Game]] =
    sans
      .foldLeft(valid[ErrorStr, List[Game]](List(game))) { (acc, move) =>
        acc andThen { games =>
          val current = games.head
          move(current.situation) map { moveOrDrop =>
            moveOrDrop.fold(game.apply, game.applyDrop) :: games
          }
        }
      }
      .map(_.reverse)

  def games(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[ErrorStr, List[Game]] =
    Parser.moves(sans) andThen { moves =>
      computeGames(makeGame(variant, initialFen), moves.value)
    }

  def gameMoveWhileValid(
      sans: Seq[SanStr],
      initialFen: Fen.Epd,
      variant: Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[ErrorStr]) =
    val init       = makeGame(variant, initialFen.some)
    val emptyGames = List.empty[(Game, Uci.WithSan)]
    Parser
      .moves(sans)
      .fold(
        err => emptyGames -> err.some,
        moves =>
          moves.value.zip(sans).foldLeft((emptyGames, none[ErrorStr])) {
            case ((games, None), (san, sanStr)) =>
              val game = games.headOption.fold(init)(_._1)
              san(game.situation).fold(
                err => (games, err.some),
                moveOrDrop => {
                  val newGame = moveOrDrop.fold(game.apply, game.applyDrop)
                  val uci     = moveOrDrop.fold(_.toUci, _.toUci)
                  ((newGame, Uci.WithSan(uci, sanStr)) :: games, None)
                }
              )
            case (end, _) => end
          }
      ) match
      case (games, err) => (init, games.reverse, err)

  private def computeSituations[M](
      sit: Situation,
      moves: List[M],
      play: M => Situation => Validated[ErrorStr, MoveOrDrop]
  ): Validated[ErrorStr, List[Situation]] =
    moves
      .foldLeft(valid[ErrorStr, List[Situation]](List(sit))) { (acc, move) =>
        acc andThen { sits =>
          val current = sits.head
          play(move)(current) map { moveOrDrop =>
            Situation(moveOrDrop.fold(_.finalizeAfter, _.finalizeAfter), !current.color) :: sits
          }
        }
      }
      .map(_.reverse)

  @scala.annotation.tailrec
  private def computeReplay(replay: Replay, ucis: List[Uci]): Validated[ErrorStr, Replay] =
    ucis match
      case Nil => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) match
          case fail: Validated.Invalid[?] => fail
          case Validated.Valid(moveOrDrop) =>
            computeReplay(replay addMove moveOrDrop, rest)

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
  ): Validated[ErrorStr, List[Board]] = situations(sans, initialFen, variant) map (_ map (_.board))

  def situations(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[ErrorStr, List[Situation]] =
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(sans) andThen { moves =>
      computeSituations[San](sit, moves.value, _.apply)
    }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[ErrorStr, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[ErrorStr, List[Situation]] =
    computeSituations[Uci](initialFenToSituation(initialFen, variant), moves, _.apply)

  def apply(
      moves: List[Uci],
      initialFen: Option[Fen.Epd],
      variant: Variant
  ): Validated[ErrorStr, Replay] =
    computeReplay(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      sans: Iterable[SanStr],
      initialFen: Option[Fen.Epd],
      variant: Variant,
      atFen: Fen.Epd
  ): Validated[ErrorStr, Ply] =
    if (Fen.read(variant, atFen).isEmpty) invalid(ErrorStr(s"Invalid Fen $atFen"))
    else

      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: Fen.Epd) = fen.value.split(' ').take(4) mkString " "
      val atFenTruncated            = truncateFen(atFen)
      def compareFen(fen: Fen.Epd)  = truncateFen(fen) == atFenTruncated

      @scala.annotation.tailrec
      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Ply): Validated[ErrorStr, Ply] =
        sans match
          case Nil => invalid(ErrorStr(s"Can't find $atFenTruncated, reached ply $ply"))
          case san :: rest =>
            san(sit) match
              case fail: Validated.Invalid[?] => fail
              case Validated.Valid(moveOrDrop) =>
                val after = moveOrDrop.fold(_.finalizeAfter, _.finalizeAfter)
                val fen   = Fen write Game(Situation(after, ply.color), ply = ply)
                if (compareFen(fen)) Validated.valid(ply)
                else recursivePlyAtFen(Situation(after, !sit.color), rest, ply + 1)

      val sit = initialFen.flatMap { Fen.read(variant, _) } | Situation(variant)

      Parser.moves(sans) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, Ply(1))
      }

  private def makeGame(variant: Variant, initialFen: Option[Fen.Epd]): Game =
    val g = Game(variant.some, initialFen)
    g.copy(startedAtPly = g.ply)
