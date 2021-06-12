package explore.model

import monocle.macros.Lenses
import explore.common.TargetObsQueries.PointingsWithObs
import explore.common.ObsQueries.ConstraintSetData
import cats.Eq
import explore.undo.UndoStacks
import lucuma.core.model.ConstraintSet

@Lenses
case class ModelUndoStacks[F[_]](
  forTargetList:    UndoStacks[F, PointingsWithObs] = UndoStacks.empty[F, PointingsWithObs],
  forConstraintSet: Map[ConstraintSet.Id, UndoStacks[F, ConstraintSetData]] = Map
    .empty[ConstraintSet.Id, UndoStacks[F, ConstraintSetData]]
    .withDefaultValue(UndoStacks.empty[F, ConstraintSetData])
)

object ModelUndoStacks {
  implicit def eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u => (u.forTargetList, u.forConstraintSet))
}
