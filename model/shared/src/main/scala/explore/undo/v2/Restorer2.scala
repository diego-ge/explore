// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo.v2

import cats.Functor
import cats.Eq

// We don't use a case class to avoid the type parameter on T
sealed trait Restorer2[F[_], M] { // M = (Local) Model
  protected implicit val functorF: Functor[F]

  type T // T = Value type

  val value: T       // Value that will be restored upon undo/redo
  val getter: M => T // How to refresh the value from the model. Used when going from undo=>redo or viceversa.
  val setter: T => M => M

  val onRestore: T => F[Unit]

  // def restore: F[Unit] = onChange(value)

  def onModel(m: M): Restorer2[F, M] =
    Restorer2[F, M, T](m, getter, setter, /*onSet,*/ onRestore)

  override def toString(): String = s"Restorer($value, ...)"
}

object Restorer2 {
  def apply[F[_], M, A](
    m:          M,
    _getter:    M => A,
    _setter:    A => M => M,
    _onRestore: A => F[Unit]
  )(implicit
    ff:         Functor[F]
  ): Restorer2[F, M] =
    new Restorer2[F, M] {
      override protected implicit val functorF = ff

      type T = A

      override val value = _getter(m)

      override val getter = _getter

      override val setter = _setter

      // override val onSet = _onSet

      override val onRestore = _onRestore
    }

  implicit def eqRestorer[F[_], M]: Eq[Restorer2[F, M]] = Eq.fromUniversalEquals
}
