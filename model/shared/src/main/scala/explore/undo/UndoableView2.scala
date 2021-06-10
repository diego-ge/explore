// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.effect.Async
import crystal.ViewF
import monocle.Lens
import explore.undo.v2.UndoContext

/**
 * Weaves `ViewF` and `UndoSetter` logic.
 *
 * When the `ViewF`'s set/mod is invoked, it will be done through the provided
 * `UndoSetter`, such that the change is pushed into the undo stack.
 *
 * The `ViewF` can be zoomed upon via the `apply` methods, which will take either
 * a `Lens` or a `get`/`mod` pair of functions, plus a side effect (`onChange`)
 * to be run whenever the resulting `ViewF`'s value changes, which can happen when
 * the value is directly `set`/`mod`, or when an `undo` or `redo` is executed. This
 * side effect could be, for example, setting the value on a remote DB.
 */
case class UndoableView2[F[_]: Async, T](
  undoCtx: UndoContext[F, T]
) {
  def apply[A](get: T => A, mod: (A => A) => T => T, onChange: A => F[Unit]): ViewF[F, A] = {
    val zoomed = undoCtx.model.zoom(get)(mod)
    ViewF[F, A](
      zoomed.get,
      undoCtx.mod[A](get, (a: A) => mod(_ => a), onChange)
    )
  }

  def apply[A](lens: Lens[T, A], onChange: A => F[Unit]): ViewF[F, A] =
    apply(lens.get, lens.modify, onChange)
}
