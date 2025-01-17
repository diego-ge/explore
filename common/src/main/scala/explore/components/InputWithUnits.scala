// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.Eq
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.validation.InputValidFormat
import lucuma.refined._
import lucuma.ui.forms.ExternalValue
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps
import react.semanticui._
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.input.IconPosition
import react.semanticui.elements.label.Label
import react.semanticui.elements.label.LabelPointing

import scala.scalajs.js

final case class InputWithUnits[EV[_], A](
  value:           EV[A],
  validFormat:     InputValidFormat[A],
  changeAuditor:   ChangeAuditor,
  icon:            js.UndefOr[ShorthandSB[Icon]] = js.undefined,
  iconPosition:    js.UndefOr[IconPosition] = js.undefined,
  id:              NonEmptyString,
  label:           js.UndefOr[ShorthandS[Label]] = js.undefined,
  units:           TagMod,
  clazz:           Css = ExploreStyles.Grow(1.refined),
  disabled:        Boolean = false,
  columnSpam:      Int Refined Interval.Closed[1, 16] = 2.refined,
  inline:          js.UndefOr[Boolean] = js.undefined,
  size:            js.UndefOr[SemanticSize] = js.undefined,
  placeholder:     js.UndefOr[String] = js.undefined
)(implicit val ev: ExternalValue[EV], val eq: Eq[A])
    extends ReactFnProps[InputWithUnits[InputWithUnits.AnyF, Any]](InputWithUnits.component)

object InputWithUnits {
  type AnyF[_]        = Any
  type Props[F[_], A] = InputWithUnits[F, A]

  val component = componentF[AnyF, Any]

  def componentF[F[_], A] =
    ScalaFnComponent[Props[F, A]] { p =>
      React.Fragment(
        FormInputEV(
          id = p.id,
          label = p.label,
          value = p.value,
          validFormat = p.validFormat,
          changeAuditor = p.changeAuditor,
          clazz = p.clazz,
          errorClazz = ExploreStyles.InputErrorTooltip,
          errorPointing = LabelPointing.Below,
          disabled = p.disabled,
          size = p.size,
          inline = p.inline,
          icon = p.icon,
          iconPosition = p.iconPosition,
          placeholder = p.placeholder
        )(p.ev, p.eq),
        <.span(
          ExploreStyles.UnitsLabel,
          p.units
        )
      )
    }

}
