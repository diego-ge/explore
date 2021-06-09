package explore.undo

import monocle.macros.Lenses
import cats.Eq

@Lenses
case class UndoStacks[F[_], M](undo: Undoer.Stack[F, M], redo: Undoer.Stack[F, M])

object UndoStacks {
  def empty[F[_], M]: UndoStacks[F, M] =
    UndoStacks(List.empty[Restorer[F, M]], List.empty[Restorer[F, M]])

  implicit def eqUndoStacks[F[_], M]: Eq[UndoStacks[F, M]] = Eq.by(s => (s.undo, s.redo))
}
