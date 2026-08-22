package chess
package tiebreak

import cats.syntax.all.*
import scalalib.zeros.given

private trait DirectEncounterImpl:

  import Tiebreak.*

  private def allTiedPlayersHaveMet(tour: Tournament, tiedPlayers: Set[Player]): Boolean =
    tiedPlayers.forall: player =>
      tiedPlayers.excl(player).subsetOf(tour.opponentsOf(player.id).toSet)

  private def missingOpponentsCount(tour: Tournament, tiedPlayers: Set[Player], player: Player): Int =
    tiedPlayers
      .excl(player)
      .count: opponent =>
        !tour.opponentsOf(player.id).contains(opponent)

  private def guaranteedTopPlayer(tour: Tournament, tiedPlayers: Set[Player]): Option[Player] =
    val directScores = tiedPlayers.view.map: player =>
      player -> tour.directScore(tiedPlayers).getOrElse(player.id, 0f)
    lazy val bounds = for
      (player, score) <- directScores
      potentialHigh = score + missingOpponentsCount(tour, tiedPlayers, player)
    yield player -> (score, potentialHigh)
    val cohortSize = tiedPlayers.size

    directScores
      .exists(_._2 > cohortSize / 2)
      .so:
        bounds
          .maxByOption(_._2._1)
          .filter:
            case (candidate, (score, _)) =>
              !bounds.exists:
                case (player, (_, potentialScore)) => candidate != player && potentialScore >= score
          ._1F

  private def playerRanks(tour: Tournament, tiedPlayers: Set[Player]): Map[PlayerId, TiebreakPoint] =
    import scala.math.Ordering.Implicits.seqOrdering

    case class RankingTask(players: Set[Player], scoreHierarchy: List[Float])

    @annotation.tailrec
    def expandAllGroups(
        pending: List[RankingTask],
        completed: Map[PlayerId, List[Float]]
    ): Map[PlayerId, List[Float]] =
      pending match
        case Nil => completed
        case task :: remaining =>
          val tied = task.players
          if tied.sizeIs <= 1 then
            expandAllGroups(remaining, completed ++ tied.map(p => p.id -> task.scoreHierarchy))
          else
            val (toSplit, resolved) = tied
              .groupBy(p => tour.directScore(tied).getOrElse(p.id, 0f))
              .partition: (_, cohort) =>
                cohort.sizeIs > 1 && cohort != tied
            val newTasks = toSplit
              .map: (score, subgroup) =>
                RankingTask(subgroup, task.scoreHierarchy :+ score)
              .toList
            val terminals = resolved.flatMap: (score, subgroup) =>
              subgroup.map(_.id -> (task.scoreHierarchy :+ score))
            expandAllGroups(newTasks ++ remaining, completed ++ terminals)

    @annotation.tailrec
    def resolveGuaranteedTop(
        remaining: Set[Player],
        resolved: List[Player]
    ): (List[Player], Set[Player]) =
      if remaining.sizeIs <= 1 then (resolved, remaining)
      else
        guaranteedTopPlayer(tour, remaining) match
          case None => (resolved, remaining)
          case Some(player) => resolveGuaranteedTop(remaining - player, resolved :+ player)

    def expandAndRank(tiedPlayers: Set[Player]): Map[PlayerId, TiebreakPoint] =
      val sorted =
        expandAllGroups(List(RankingTask(tiedPlayers, Nil)), Map.empty).toList.sortBy(_._2.map(-_))
      val rankByHierarchy = sorted
        .map(_._2)
        .zipWithIndex
        .foldLeft(Map.empty[List[Float], Int]):
          case (acc, (hierarchy, idx)) => acc.updatedWith(hierarchy)(_.orElse(Some(idx + 1)))
      sorted
        .map: (playerId, hierarchy) =>
          playerId -> TiebreakPoint(rankByHierarchy.getOrElse(hierarchy, 0))
        .toMap

    if tiedPlayers.sizeIs <= 1 then tiedPlayers.map(p => p.id -> TiebreakPoint.zero).toMap
    else if allTiedPlayersHaveMet(tour, tiedPlayers) then expandAndRank(tiedPlayers)
    else
      val (guaranteedTop, unresolved) = resolveGuaranteedTop(tiedPlayers, List.empty)
      val guaranteedRanks = guaranteedTop
        .mapWithIndex: (player, idx) =>
          player.id -> TiebreakPoint(idx + 1)
        .toMap
      val unresolvedRanks = if allTiedPlayersHaveMet(tour, unresolved) then
        val rankOffset = guaranteedTop.size
        expandAndRank(unresolved).view.mapValues(p => TiebreakPoint(p.value + rankOffset)).toMap
      else unresolved.map(_.id -> TiebreakPoint.zero).toMap
      (guaranteedRanks ++ unresolvedRanks)

  def compute(tour: Tournament, previousPoints: PlayerPoints): PlayerPoints =
    val builder = Map.newBuilder[PlayerId, List[TiebreakPoint]]
    tour.players
      .groupBy(p => (tour.scoreOf(p.id), previousPoints.get(p.id)))
      .foreach: (_, tiedPlayers) =>
        val ranks = playerRanks(tour, tiedPlayers)
        tiedPlayers.foreach: player =>
          val rank = ranks.getOrElse(player.id, TiebreakPoint.zero)
          builder.addOne(player.id -> (previousPoints.getOrElse(player.id, Nil) :+ rank))
    builder.result()
