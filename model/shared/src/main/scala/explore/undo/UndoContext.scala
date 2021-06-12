package explore.undo

import cats.syntax.all._
import crystal.ViewF
import cats.Monad
import crystal.implicits._

trait UndoSetter[F[_], M] {
  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => F[Unit],
    onRestore: (M, A) => F[Unit]
  )(v:         A): F[Unit]

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => F[Unit],
    onRestore: A => F[Unit]
  )(v:         A): F[Unit] =
    set(getter, setter, (_: M, a: A) => onSet(a), (_: M, a: A) => onRestore(a))(v)

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => F[Unit]
  )(v:         A): F[Unit] =
    set(getter, setter, onSet, onSet)(v)

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => F[Unit]
  )(v:         A): F[Unit] =
    set(getter, setter, (_: M, a: A) => onSet(a))(v)

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => F[Unit],
    onRestore: (M, A) => F[Unit]
  )(f:         A => A): F[Unit]

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     A => F[Unit],
    onRestore: A => F[Unit]
  )(f:         A => A): F[Unit] =
    mod(getter, setter, (_: M, a: A) => onSet(a), (_: M, a: A) => onRestore(a))(f)

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => F[Unit]
  )(f:         A => A): F[Unit] =
    mod(getter, setter, onSet, onSet)(f)

  def mod[A](
    getter: M => A,
    setter: A => M => M,
    onSet:  A => F[Unit]
  )(f:      A => A): F[Unit] =
    mod(getter, setter, (_: M, a: A) => onSet(a))(f)
}

case class UndoContext[F[_]: Monad, M](stacks: ViewF[F, UndoStacks[F, M]], model: ViewF[F, M])
    extends UndoSetter[F, M] {
  private lazy val undoStack: ViewF[F, UndoStack[F, M]] = stacks.zoom(UndoStacks.undo)
  private lazy val redoStack: ViewF[F, UndoStack[F, M]] = stacks.zoom(UndoStacks.redo)

  lazy val isUndoEmpty: Boolean = stacks.get.undo.isEmpty
  lazy val isRedoEmpty: Boolean = stacks.get.redo.isEmpty

  lazy val working: Boolean = stacks.get.working

  private def push(stack: ViewF[F, UndoStack[F, M]]): Restorer[F, M] => F[Unit] =
    restorer => stack.mod(s => restorer +: s)

  // Try doing both state updates with .start and removing the working mechanism
  // We have to reorganize things.
  // Move stack reorganizing logic to UndoStacks, produce the new stacks but don't modify until here.

  private def undoStacks: F[Option[Restorer[F, M]]] =
    stacks.modAndExtract(ss =>
      ss.undo match {
        // HERE on head.onModel(model.get) Sometimes obtaining wrong value!!!!
        case head :: tail =>
          // (UndoStacks(tail, head.onModel(model.get) +: ss.redo, true), head.some)
          (UndoStacks(tail, head.onModel(model.get) +: ss.redo, false), head.some)
        case _            => (ss, none)
      }
    )

  private def redoStacks: F[Option[Restorer[F, M]]] =
    stacks.modAndExtract(ss =>
      ss.redo match {
        case head :: tail =>
          // (UndoStacks(head.onModel(model.get) +: ss.undo, tail, true), head.some)
          (UndoStacks(head.onModel(model.get) +: ss.undo, tail, false), head.some)
        case _            => (ss, none)
      }
    )

  // private def pop(stack: ViewF[F, UndoStack[F, M]]): F[Option[Restorer2[F, M]]] =
  //   // TODO It'd be nice to have a modify in ViewF like Ref has, allowing to extract something from old value (head in this case) that's not present in the new value.
  //   stack.get match {
  //     case head :: tail => stack.set(tail).as(head.some)
  //     case _            => none.pure[F]
  //   }

  private def reset(stack: ViewF[F, UndoStack[F, M]]): F[Unit] =
    stack.set(List.empty)

  // private def restore(
  //   fromStack: ViewF[F, UndoStack[F, M]],
  //   toStack:   ViewF[F, UndoStack[F, M]]
  // ): F[Unit] =
  //   pop(fromStack).flatMap(
  //     _.map(restorer =>
  //       push(toStack)(restorer.onModel(model.get)) >>
  //         model.mod(restorer.setter(restorer.value)) >>
  //         restorer.onRestore(restorer.value)
  //     ).orUnit
  //   )

  def restore(restorerOpt: Option[Restorer[F, M]]): F[Unit] =
    restorerOpt
      .map(restorer =>
        model.mod(restorer.setter(restorer.value)) >>
          restorer.onRestore(model.get, restorer.value) // >>
      // stacks.zoom(UndoStacks.working[F, M]).set(false)
      )
      .orUnit

  def set[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => F[Unit],
    onRestore: (M, A) => F[Unit]
  )(v:         A): F[Unit] =
    for {
      _ <- push(undoStack)(Restorer[F, M, A](model.get, getter, setter, onRestore))
      _ <- reset(redoStack)
      _ <- model.mod.compose(setter)(v)
      _ <- onSet(model.get, v)
    } yield ()

  def mod[A](
    getter:    M => A,
    setter:    A => M => M,
    onSet:     (M, A) => F[Unit],
    onRestore: (M, A) => F[Unit]
  )(f:         A => A): F[Unit] =
    set(getter, setter, onSet, onRestore)(f(getter(model.get)))

  val undo: F[Unit] = undoStacks >>= restore

  val redo: F[Unit] = redoStacks >>= restore
}

// case class UndoableAction[F[_], M](
//   mod:  Mod[F, M] => (M => M) => F[Unit],
//   undo: Mod[F, M] => F[Unit],
//   redo: Mod[F, M] => F[Unit]
// ) {
//   val set: M => F[Unit] = m =>
// }

// case class Action[F[_], M, A](get: M => A, mod: (A => A) => M => F[Unit]
