package explore.model

import monocle.macros.Lenses
import explore.common.TargetObsQueries.PointingsWithObs
import explore.common.ConstraintsQueries.ConstraintSetModel
import cats.Eq
import explore.undo.UndoStacks
import lucuma.core.model.ConstraintSet

@Lenses
case class ModelUndoStacks[F[_]](
  forTargetList:    UndoStacks2[F, PointingsWithObs] = UndoStacks2.empty[F, PointingsWithObs],
  forConstraintSet: Map[ConstraintSet.Id, UndoStacks2[F, ConstraintSetModel]] = Map
    .empty[ConstraintSet.Id, UndoStacks2[F, ConstraintSetModel]]
    .withDefaultValue(UndoStacks2.empty[F, ConstraintSetModel])
)

object ModelUndoStacks {
  implicit def eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] =
    Eq.by(u => (u.forTargetList, u.forConstraintSet))
}
