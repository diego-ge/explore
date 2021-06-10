package explore.undo.v2

import monocle.macros.Lenses
import cats.Eq

@Lenses
case class UndoStacks2[F[_], M](
  undo:    UndoStack[F, M],
  redo:    UndoStack[F, M],
  working: Boolean
)

object UndoStacks2 {
  def empty[F[_], M]: UndoStacks2[F, M] =
    UndoStacks2(List.empty[Restorer2[F, M]], List.empty[Restorer2[F, M]], false)

  implicit def eqUndoStacks[F[_], M]: Eq[UndoStacks2[F, M]] =
    Eq.by(s => (s.undo, s.redo, s.working))
}
