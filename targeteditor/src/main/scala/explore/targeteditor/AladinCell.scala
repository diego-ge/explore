// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.Icons
import explore.View
import explore.components.ui.ExploreStyles
import explore.model.ModelOptics
import explore.model.TargetVisualOptions
import explore.model.reusability._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.aladin.Fov
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.modules.popup.Popup
import react.semanticui.sizes._

final case class AladinCell(
  target:  View[Coordinates],
  options: View[TargetVisualOptions]
) extends ReactProps[AladinCell](AladinCell.component) {
  val aladinCoords: Coordinates = target.get
}

object AladinCell extends ModelOptics {
  type Props = AladinCell
  val AladinRef = AladinContainer.component

  @Lenses
  final case class State(fov: Fov, current: Coordinates)

  object State {
    val Zero = State(Fov(Angle.Angle0, Angle.Angle0), Coordinates.Zero)
  }
  implicit val propsReuse = Reusability.derive[Props]
  implicit val stateReuse = Reusability.never[State]

  class Backend($ : BackendScope[Props, State]) {
    // Create a mutable reference
    private val aladinRef = Ref.toScalaComponent(AladinRef)

    val centerOnTarget =
      aladinRef.get
        .flatMapCB(_.backend.centerOnTarget)
        .toCallback

    val gotoRaDec = (coords: Coordinates) =>
      aladinRef.get
        .flatMapCB(_.backend.gotoRaDec(coords))
        .toCallback

    def render(props: Props, state: State) =
      React.Fragment(
        <.div(
          ExploreStyles.TargetAladinCell,
          <.div(
            ExploreStyles.AladinContainerColumn,
            AladinRef
              .withRef(aladinRef) {
                AladinContainer(
                  props.target,
                  props.options.get,
                  $.setStateL(State.current)(_),
                  $.setStateL(State.fov)(_)
                )
              },
            AladinToolbar(state.fov, state.current),
            <.div(
              ExploreStyles.AladinCenterButton,
              Popup(
                content = "Center on target",
                trigger = Button(size = Mini, icon = true, onClick = centerOnTarget)(Icons.Bullseye)
              )
            )
          )
        )
      )

    def newProps(currentProps: Props, nextProps: Props): Callback =
      gotoRaDec(nextProps.aladinCoords)
        .when(nextProps.aladinCoords =!= currentProps.aladinCoords)
        .void
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialStateFromProps(p => State.Zero.copy(current = p.aladinCoords))
      .renderBackend[Backend]
      .componentDidUpdate($ => $.backend.newProps($.prevProps, $.currentProps))
      .configure(Reusability.shouldComponentUpdate)
      .build

}