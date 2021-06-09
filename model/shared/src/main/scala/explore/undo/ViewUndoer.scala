package explore.undo

import cats.effect.Sync
import crystal.ViewF

case class ViewUndoer[F[_]: Sync, M](protected val stacksView: ViewF[F, UndoStacks[F, M]])
    extends Undoer[F, M] {
  type Stacks = UndoStacks[F, M]

  lazy val getStacks: F[Stacks] = Sync[F].delay(stacksView.get)

  def modStacks(mod: Stacks => Stacks): F[Unit] = stacksView.mod(mod)

  lazy val undoStack: StackLens = UndoStacks.undo

  lazy val redoStack: StackLens = UndoStacks.redo
}
