package explore.undo

import cats.syntax.all._
import cats.effect.Ref
import cats.effect.Sync

// We are not using this for the moment, but we might.
trait RefUndoer[F[_], M] extends Undoer[F, M] {
  type Stacks = UndoStacks[F, M]

  protected val stacks: Ref[F, Stacks]

  val getStacks: F[Stacks] = stacks.get

  def modStacks(mod: Stacks => Stacks): F[Unit] = stacks.update(mod)

  val undoStack: StackLens = UndoStacks.undo

  val redoStack: StackLens = UndoStacks.redo

}

object RefUndoer {
  def apply[F[_]: Sync, M]: F[RefUndoer[F, M]] =
    Ref[F]
      .of(UndoStacks.empty[F, M])
      .map(stacks_ => new RefUndoer[F, M] { override protected val stacks = stacks_ })
}
