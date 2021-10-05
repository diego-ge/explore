// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Applicative
import cats.effect.Async
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import explore.common.ObsQueriesGQL._
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.Focused
import explore.model.ObsSummaryWithTargetsAndConstraints
import explore.optics.GetAdjust
import explore.undo.Action
import explore.undo.KIListMod
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import monocle.Focus

object ObsListActions {
  protected val obsListMod =
    KIListMod[ObsSummaryWithTargetsAndConstraints, Observation.Id](
      ObsSummaryWithTargetsAndConstraints.id
    )

  private def obsWithId(
    obsId: Observation.Id
  ): GetAdjust[KeyedIndexedList[Observation.Id, ObsSummaryWithTargetsAndConstraints], Option[
    ObsSummaryWithTargetsAndConstraints
  ]] =
    obsListMod
      .withKey(obsId)
      .composeOptionLens(Focus[(ObsSummaryWithTargetsAndConstraints, Int)](_._1))

  def obsStatus[F[_]: Applicative](obsId: Observation.Id)(implicit
    c:                                    TransactionalClient[F, ObservationDB]
  ) = Action[F](
    access = obsWithId(obsId).composeOptionLens(ObsSummaryWithTargetsAndConstraints.status)
  )(onSet =
    (_, status) =>
      UpdateObservationMutation
        .execute[F](
          EditObservationInput(observationId = obsId, status = status.orIgnore)
        )
        .void
  )

  def obsActiveStatus[F[_]: Applicative](obsId: Observation.Id)(implicit
    c:                                          TransactionalClient[F, ObservationDB]
  ) = Action[F](
    access = obsWithId(obsId).composeOptionLens(ObsSummaryWithTargetsAndConstraints.activeStatus)
  )(onSet =
    (_, activeStatus) =>
      UpdateObservationMutation
        .execute[F](
          EditObservationInput(observationId = obsId, activeStatus = activeStatus.orIgnore)
        )
        .void
  )

  def obsExistence[F[_]: Async](obsId: Observation.Id, focused: View[Option[Focused]])(implicit
    c:                                 TransactionalClient[F, ObservationDB]
  ) =
    Action[F](
      access = obsListMod.withKey(obsId)
    )(
      onSet = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          ProgramDeleteObservation.execute[F](obsId).void
        } { case (obs, _) =>
          ProgramCreateObservation
            .execute[F](CreateObservationInput(programId = "p-2", observationId = obs.id.assign))
            .void >>
            focused.set(Focused.FocusedObs(obs.id).some).to[F]
        },
      onRestore = (_, elemWithIndexOpt) =>
        elemWithIndexOpt.fold {
          ProgramDeleteObservation.execute[F](obsId).void
        } { case (obs, _) =>
          ProgramUndeleteObservation.execute[F](obs.id).void >>
            focused.set(Focused.FocusedObs(obs.id).some).to[F]
        }
    )
}
