package explore.model

import monocle.macros.Lenses
import explore.common.TargetObsQueries.PointingsWithObs
import explore.common.ObsQueries.ConstraintSetData
import explore.common.ObsQueries.ObservationList
import explore.common.TargetQueries.TargetResult
import cats.Eq
import explore.undo.UndoStacks
import lucuma.core.model.Observation
import lucuma.core.model.Target

@Lenses
case class ModelUndoStacks[F[_]](
  forObsList:       UndoStacks[F, ObservationList] = UndoStacks.empty[F, ObservationList],
  forTargetList:    UndoStacks[F, PointingsWithObs] = UndoStacks.empty[F, PointingsWithObs],
  forTarget:        Map[Target.Id, UndoStacks[F, TargetResult]] = Map
    .empty[Target.Id, UndoStacks[F, TargetResult]]
    .withDefaultValue(UndoStacks.empty[F, TargetResult]),
  forConstraintSet: Map[Observation.Id, UndoStacks[F, ConstraintSetData]] = Map
    .empty[Observation.Id, UndoStacks[F, ConstraintSetData]]
    .withDefaultValue(UndoStacks.empty[F, ConstraintSetData])
)

object ModelUndoStacks {
  implicit def eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u => (u.forTargetList, u.forConstraintSet))
}
