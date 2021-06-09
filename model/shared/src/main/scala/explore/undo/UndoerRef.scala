package explore.undo

import cats.syntax.all._
import cats.effect.Ref
import cats.effect.Sync
import monocle.macros.Lenses

trait UndoerRef[F[_], M] extends Undoer[F, M] {
  type Stacks = UndoerRef.Stacks[F, M]

  protected val stacks: Ref[F, Stacks]

  val getStacks: F[Stacks] = stacks.get

  def modStacks(mod: Stacks => Stacks): F[Unit] = stacks.update(mod)

  val undoStack: StackLens = UndoerRef.Stacks.undo

  val redoStack: StackLens = UndoerRef.Stacks.redo

}

object UndoerRef {
  @Lenses
  case class Stacks[F[_], M](undo: Undoer.Stack[F, M], redo: Undoer.Stack[F, M])

  def apply[F[_]: Sync, M]: F[UndoerRef[F, M]] =
    Ref[F]
      .of(Stacks(List.empty[Restorer[F, M]], List.empty[Restorer[F, M]]))
      .map(stacks_ => new UndoerRef[F, M] { override protected val stacks = stacks_ })
}
