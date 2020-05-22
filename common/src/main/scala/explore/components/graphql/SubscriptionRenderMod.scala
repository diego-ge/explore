// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import scala.concurrent.duration._
import scala.language.postfixOps

import cats.effect.ConcurrentEffect
import cats.effect.IO
import cats.effect.Timer
import cats.implicits._
import clue.GraphQLStreamingClient
import crystal.View
import crystal.data.Pot
import crystal.data.react._
import crystal.react.StreamRendererMod
import crystal.react.implicits._
import explore.model.reusability
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.message.Message
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class SubscriptionRenderMod[D, A](
  subscribe:         IO[GraphQLStreamingClient[IO]#Subscription[D]],
  streamModifier:    fs2.Stream[IO, D] => fs2.Stream[IO, A] = identity[fs2.Stream[IO, D]] _
)(
  val valueRender:   View[IO, A] => VdomNode,
  val pendingRender: Long => VdomNode = (_ => Icon(name = "spinner", loading = true, size = Large)),
  val errorRender:   Throwable => VdomNode = (t => Message(error = true)(t.getMessage)),
  val onNewData:     IO[Unit] = IO.unit
)(implicit
  val ce:            ConcurrentEffect[IO],
  val timer:         Timer[IO],
  val logger:        Logger[IO],
  val reuse:         Reusability[A]
) extends ReactProps(SubscriptionRenderMod.component)
    with SubscriptionRenderMod.Props[IO, D, A]

object SubscriptionRenderMod {
  trait Props[F[_], D, A] {
    val subscribe: F[GraphQLStreamingClient[F]#Subscription[D]]
    val streamModifier: fs2.Stream[F, D] => fs2.Stream[F, A]
    val valueRender: View[F, A] => VdomNode
    val pendingRender: Long => VdomNode
    val errorRender: Throwable => VdomNode
    val onNewData: F[Unit]
    implicit val ce: ConcurrentEffect[F]
    implicit val timer: Timer[F]
    implicit val logger: Logger[F]
    implicit val reuse: Reusability[A]
  }

  protected final case class State[F[_], D, A](
    subscription: Option[
      GraphQLStreamingClient[F]#Subscription[D]
    ] = None,
    renderer:     Option[StreamRendererMod.Component[F, A]] = None
  )

  implicit protected def propsReuse[F[_], D, A]: Reusability[Props[F, D, A]] = Reusability.always
  implicit protected def stateReuse[F[_], D, A]: Reusability[State[F, D, A]] = Reusability.never

  protected def componentBuilder[F[_], D, A] =
    ScalaComponent
      .builder[Props[F, D, A]]
      .initialState(State[F, D, A]())
      .render { $ =>
        React.Fragment(
          $.state.renderer.fold[VdomNode](EmptyVdom)(
            _ { view =>
              view.get.fold($.props.pendingRender,
                            $.props.errorRender,
                            value => $.props.valueRender(view.zoom(_ => value)(f => _.map(f)))
              )
            }
          )
        )
      }
      .componentDidMount { $ =>
        implicit val ce     = $.props.ce
        implicit val timer  = $.props.timer
        implicit val logger = $.props.logger
        implicit val reuse  = $.props.reuse

        $.props.subscribe.flatMap { subscription =>
          $.setStateIn[F](
            State(
              subscription.some,
              StreamRendererMod
                .build(
                  $.props
                    .streamModifier(subscription.stream)
                    .flatTap(_ => fs2.Stream.eval($.props.onNewData)),
                  holdAfterMod = (2 seconds).some
                )
                .some
            )
          )
        }.runInCB
      }
      .componentWillUnmount { $ =>
        implicit val ce = $.props.ce

        $.state.subscription.map(_.stop.runInCB).getOrEmpty
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any]
}
