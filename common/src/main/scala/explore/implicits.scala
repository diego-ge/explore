// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats._
import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.effect.kernel.Resource
import cats.syntax.all._
import clue._
import coulomb.Quantity
import crystal.Pot
import crystal.ViewF
import crystal.ViewOptF
import crystal.implicits._
import crystal.react.ReuseViewF
import crystal.react.ReuseViewOptF
import crystal.react.reuse._
import eu.timepit.refined.api.Refined
import explore.model.AppContext
import explore.optics._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom._
import lucuma.core.optics._
import lucuma.core.util.Enumerated
import lucuma.schemas._
import monocle.function.At
import monocle.function.At.at
import monocle.function.Index.index
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import queries.schemas._

import scala.annotation.unused
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

trait ListImplicits {
  def unzip4[A, B, C, D](list: List[(A, B, C, D)]): (List[A], List[B], List[C], List[D]) =
    list.foldRight((List.empty[A], List.empty[B], List.empty[C], List.empty[D]))((tuple, accum) =>
      (tuple._1 :: accum._1, tuple._2 :: accum._2, tuple._3 :: accum._3, tuple._4 :: accum._4)
    )

  implicit class ViewListOps[F[_]: Monad, A](val viewList: ViewF[F, List[A]]) {
    def toListOfViews: List[ViewF[F, A]] =
      // It's safe to "get" since we are only invoking for existing indices.
      viewList.get.indices.toList.map { i =>
        val atIndex = index[List[A], Int, A](i)
        viewList
          .zoom((atIndex.getOption _).andThen(_.get))(atIndex.modify)
      }
  }

  implicit class ViewMapOps[F[_]: Monad, K, V](val viewMap: ViewF[F, Map[K, V]]) {
    def toListOfViews: List[(K, ViewF[F, V])] =
      // It's safe to "get" since we are only invoking for existing keys.
      viewMap.get.keys.toList.map(k =>
        k -> viewMap.zoom(at[Map[K, V], K, Option[V]](k)).zoom(_.get)(f => _.map(f))
      )
  }

}

trait ContextImplicits {
  implicit def appContext2Parallel[F[_]](implicit ctx: AppContext[F]): Parallel[F] =
    ctx.P
  implicit def appContext2Logger[F[_]](implicit ctx: AppContext[F]): Logger[F]     =
    ctx.logger
  implicit def appContext2UserPreferencesDBClient[F[_]](implicit
    ctx: AppContext[F]
  ): WebSocketClient[F, UserPreferencesDB] =
    ctx.clients.preferencesDB
  implicit def appContext2ODBClient[F[_]](implicit
    ctx: AppContext[F]
  ): WebSocketClient[F, ObservationDB] =
    ctx.clients.odb
  implicit def appContext2ITC[F[_]](implicit
    ctx: AppContext[F]
  ): TransactionalClient[F, ITC] =
    ctx.clients.itc
}

trait RefinedImplicits {
  @inline
  implicit def vdomNodeFromRefined[T, P](v: T Refined P)(implicit
    f:                                      T => VdomNode
  ): VdomNode =
    f(v.value)
}

trait EnumeratedImplicits {
  implicit def zipEnums[A: Enumerated, B: Enumerated]: Enumerated[(A, B)] =
    Enumerated
      .fromNEL(NonEmptyList.fromListUnsafe((Enumerated[A].all, Enumerated[B].all).tupled))
      .withTag { case (a, b) => s"${Enumerated[A].tag(a)}, ${Enumerated[B].tag(b)} " }
}

object implicits
    extends ShorthandTypes
    with ListImplicits
    with ContextImplicits
    with RefinedImplicits
    with EnumeratedImplicits {
  // View Optics implicits
  implicit class ViewOpticsOps[F[_], A](val view: ReuseViewF[F, A]) extends AnyVal {
    def zoomGetAdjust[B](getAdjust: GetAdjust[A, B])(implicit F: Monad[F]): ReuseViewF[F, B] =
      view.zoom(getAdjust.get)(getAdjust.mod)

    // Helps type inference by sidestepping overloaded "zoom".
    def zoomPrism[B](prism: monocle.Prism[A, B])(implicit F: Monad[F]): ReuseViewOptF[F, B] =
      view.zoom(prism)

    // Helps type inference by sidestepping overloaded "zoom".
    def zoomLens[B](lens: monocle.Lens[A, B])(implicit F: Monad[F]): ReuseViewF[F, B] =
      view.zoom(lens)
  }

  implicit class ViewOptOpticsOps[F[_], A](val viewOpt: ReuseViewOptF[F, A]) extends AnyVal {
    // Helps type inference by sidestepping overloaded "zoom".
    def zoomLens[B](lens: monocle.Lens[A, B])(implicit F: Monad[F]): ReuseViewOptF[F, B] =
      viewOpt.zoom(lens)
  }

  // Coulomb implicits
  implicit class CoulombViewOps[F[_], N, U](val self: ViewF[F, Quantity[N, U]]) extends AnyVal {
    def stripQuantity: ViewF[F, N] = self.as(quantityIso[N, U])
  }

  implicit class CoulombViewOptOps[F[_], N, U](val self: ViewOptF[F, Quantity[N, U]])
      extends AnyVal {
    def stripQuantity: ViewOptF[F, N] = self.as(quantityIso[N, U])
  }

  implicit class CoulombReuseViewOptOps[F[_], N, U](val self: ReuseViewOptF[F, Quantity[N, U]])
      extends AnyVal {
    def stripQuantity(implicit F: Monad[F]): ReuseViewOptF[F, N] = self.as(quantityIso[N, U])
  }

  // React implicits
  implicit class HtmlAttrsOps(val a: HtmlAttrs) extends AnyVal {
    // Generalize https://gist.github.com/pstoica/4323d3e6e37e8a23dd59
    def onComponentBlur(handler: Callback): TagMod =
      a.onBlur ==> { (e: ReactFocusEvent) =>
        val currentTarget = e.currentTarget

        handler
          .when_(!currentTarget.contains(dom.document.activeElement))
          .setTimeout(Duration.Zero)
          .void
      }
  }

  implicit class EffectOps[F[_], A](val f: F[A]) extends AnyVal {

    def attemptPot(implicit F: MonadThrow[F]): F[Pot[A]] = f.attempt.map(_.toTry.some.toPot)

    /**
     * Given an effect producing an A and a signal stream, runs the effect and then re-runs it
     * whenver a signal is received, producing a Stream[A].
     */
    def reRunOnSignal(
      signal:     fs2.Stream[F, Unit],
      debounce:   Option[FiniteDuration] = 2.seconds.some
    )(implicit F: Temporal[F]): fs2.Stream[F, A] = {
      val debouncedSignal = debounce.fold(signal)(signal.debounce)
      fs2.Stream.eval(f) ++ debouncedSignal.evalMap(_ => f)
    }

    /**
     * Given an effect producing an A and a bunch of signal streams, runs the effect and then
     * re-runs it whenver a signal is received, producing a Stream[A].
     */
    def reRunOnSignals(
      signals:    NonEmptyList[fs2.Stream[F, Unit]],
      debounce:   Option[FiniteDuration] = 2.seconds.some
    )(implicit F: Temporal[F]): fs2.Stream[F, A] =
      reRunOnSignal(signals.reduceLeft(_ merge _), debounce)

    def reRunOnResourceSignals(
      subscriptions: NonEmptyList[Resource[F, fs2.Stream[F, ?]]],
      debounce:      Option[FiniteDuration] = 2.seconds.some
    )(implicit F:    Temporal[F]): Resource[F, fs2.Stream[F, A]] =
      subscriptions.sequence
        .map(ss => reRunOnSignals(ss.map(_.void), debounce))

    def reRunOnResourceSignals(
      head:       Resource[F, fs2.Stream[F, ?]],
      tail:       Resource[F, fs2.Stream[F, ?]]*
    )(implicit F: Temporal[F]): Resource[F, fs2.Stream[F, A]] =
      reRunOnResourceSignals(NonEmptyList.of(head, tail: _*))

    def reRunOnResourceSignals(
      debounce:   FiniteDuration,
      head:       Resource[F, fs2.Stream[F, ?]],
      tail:       Resource[F, fs2.Stream[F, ?]]*
    )(implicit F: Temporal[F]): Resource[F, fs2.Stream[F, A]] =
      reRunOnResourceSignals(NonEmptyList.of(head, tail: _*), debounce.some)
  }

  implicit class PotEffectOps[F[_], A](val f: F[Pot[A]]) extends AnyVal {
    def resetOnSignal(
      signal:     fs2.Stream[F, Unit],
      debounce:   Option[FiniteDuration] = 2.seconds.some
    )(implicit F: Temporal[F]): fs2.Stream[F, Pot[A]] = {
      val debouncedSignal = debounce.fold(signal)(signal.debounce)
      fs2.Stream.eval(f) ++ debouncedSignal.flatMap(_ =>
        fs2.Stream(Pot.pending) ++ fs2.Stream.eval(f)
      )
    }

    def resetOnSignals(
      signals:    NonEmptyList[fs2.Stream[F, Unit]],
      debounce:   Option[FiniteDuration] = 2.seconds.some
    )(implicit F: Temporal[F]): fs2.Stream[F, Pot[A]] =
      resetOnSignal(signals.reduceLeft(_ merge _), debounce)

    private def resetOnResourceSignalsB(
      subscriptions: NonEmptyList[Resource[F, fs2.Stream[F, ?]]],
      debounce:      Option[FiniteDuration] = 2.seconds.some
    )(implicit F:    Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
      subscriptions.sequence
        .map(ss => resetOnSignals(ss.map(_.void), debounce))

    def resetOnResourceSignals(
      head:       Resource[F, fs2.Stream[F, ?]],
      tail:       Resource[F, fs2.Stream[F, ?]]*
    )(implicit F: Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
      resetOnResourceSignalsB(NonEmptyList.of(head, tail: _*))

    def resetOnResourceSignals(
      debounce:   FiniteDuration,
      head:       Resource[F, fs2.Stream[F, ?]],
      tail:       Resource[F, fs2.Stream[F, ?]]*
    )(implicit F: Temporal[F]): Resource[F, fs2.Stream[F, Pot[A]]] =
      resetOnResourceSignalsB(NonEmptyList.of(head, tail: _*), debounce.some)
  }
}
