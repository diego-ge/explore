package explore.undo

import monocle.macros.Lenses
import cats.Eq

@Lenses
case class UndoStacks[F[_], M](
  undo:    UndoStack[F, M],
  redo:    UndoStack[F, M],
  working: Boolean
)
object UndoStacks {
  def empty[F[_], M]: UndoStacks[F, M] =
    UndoStacks(List.empty[Restorer[F, M]], List.empty[Restorer[F, M]], false)

  implicit def eqUndoStacks[F[_], M]: Eq[UndoStacks[F, M]] =
    Eq.by(s => (s.undo, s.redo, s.working))
}
