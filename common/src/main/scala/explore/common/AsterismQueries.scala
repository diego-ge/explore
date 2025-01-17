// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.effect.Async
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithConstraintsAndConf
import explore.model.TargetWithObs
import explore.model.syntax.all._
import japgolly.scalajs.react._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter
import queries.common.AsterismQueriesGQL._
import queries.common.ObsQueriesGQL._
import queries.schemas.implicits._

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object AsterismQueries {
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  // This is used for sorting the AsterismGroupObsList. If we change to sort by name or something
  // else, we can remove this.
  implicit val orderSortedSet: Order[ObsIdSet] = ObsIdSet.orderObsIdSet

  type ObservationResult = AsterismGroupObsQuery.Data.Observations.Matches
  val ObservationResult = AsterismGroupObsQuery.Data.Observations.Matches

  type AsterismGroupList = SortedMap[ObsIdSet, AsterismGroup]
  type TargetWithObsList = SortedMap[Target.Id, TargetWithObs]
  type ObsList           = SortedMap[Observation.Id, ObsSummaryWithConstraintsAndConf]

  case class AsterismGroupsWithObs(
    asterismGroups: AsterismGroupList,
    targetsWithObs: TargetWithObsList,
    observations:   ObsList
  )

  object AsterismGroupsWithObs {
    val asterismGroups = Focus[AsterismGroupsWithObs](_.asterismGroups)
    val targetsWithObs = Focus[AsterismGroupsWithObs](_.targetsWithObs)
    val observations   = Focus[AsterismGroupsWithObs](_.observations)
  }

  // Some helper methods on AsterismGroupList
  implicit class AsterismGroupListOps(val self: AsterismGroupList) extends AnyVal {
    def findContainingObsIds(obsIds: ObsIdSet): Option[AsterismGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(_._2)

    def findWithTargetIds(targetIds: SortedSet[Target.Id]): Option[AsterismGroup] =
      self.find { case (_, ag) => ag.targetIds === targetIds }.map(_._2)
  }

  implicit val asterismGroupWithObsReuse: Reusability[AsterismGroupsWithObs] =
    Reusability.derive

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithConstraintsAndConf =
    ObsSummaryWithConstraintsAndConf(
      obsR.id,
      obsR.constraintSet,
      obsR.status,
      obsR.activeStatus,
      obsR.plannedTime.execution,
      obsR.targetEnvironment.asterism.map(_.id).toSet,
      obsR.scienceMode,
      obsR.visualizationTime,
      obsR.posAngleConstraint,
      obsR.scienceRequirements.spectroscopy.wavelength
    )

  private val queryToAsterismGroupWithObsGetter
    : Getter[AsterismGroupObsQuery.Data, AsterismGroupsWithObs] = data => {
    val asterismGroups = data.asterismGroup.matches
      .map { mtch =>
        ObsIdSet.fromList(mtch.observationIds).map { obsIdSet =>
          AsterismGroup(obsIdSet, SortedSet.from(mtch.asterism.map(_.id)))
        }
      }
      .flatten
      .toSortedMap(_.obsIds)

    val targetsWithObs = data.targetGroup.matches.toSortedMap(_.id, _.targetWithObs)

    AsterismGroupsWithObs(
      asterismGroups,
      targetsWithObs,
      data.observations.matches
        .map(obsResultToSummary)
        .toSortedMap(ObsSummaryWithConstraintsAndConf.id.get)
    )
  }

  implicit class AsterismGroupObsQueryDataOps(val self: AsterismGroupObsQuery.Data.type)
      extends AnyVal {
    def asAsterismGroupWithObs = queryToAsterismGroupWithObsGetter
  }

  def replaceAsterism[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetIds:  List[Target.Id]
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = UpdateObservationsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        targetEnvironment = TargetEnvironmentInput(asterism = targetIds.assign).assign
      )
    )
    UpdateObservationMutation.execute[F](input).void
  }

  def addTargetToAsterisms[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetId:   Target.Id
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = List(EditAsterismsPatchInput(ADD = targetId.assign))
    )
    UpdateAsterismsMutation.execute[F](input).void
  }

  def removeTargetFromAsterisms[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetId:   Target.Id
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = UpdateAsterismsInput(
      WHERE = obsIds.toWhereObservation.assign,
      SET = List(EditAsterismsPatchInput(DELETE = targetId.assign))
    )
    UpdateAsterismsMutation.execute[F](input).void
  }
}
