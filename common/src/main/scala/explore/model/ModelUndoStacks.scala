package explore.model

import monocle.macros.Lenses
import explore.common.TargetObsQueries.PointingsWithObs
import cats.Eq
import explore.undo.v2.UndoStacks2

@Lenses
case class ModelUndoStacks[F[_]](
  targetListUndoer: UndoStacks2[F, PointingsWithObs] = UndoStacks2.empty[F, PointingsWithObs]
)

object ModelUndoStacks {
  implicit def eqModelUndoStacks[F[_]]: Eq[ModelUndoStacks[F]] = Eq.by(u => u.targetListUndoer)
}
