// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.syntax

import cats.Eq
import cats.syntax.all._
import explore.components.InputWithUnits
import explore.components.ui.ExploreStyles
import explore.model.Constants
import explore.utils._
import japgolly.scalajs.react.CtorType
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.component.Scala
import japgolly.scalajs.react.component.ScalaFn
import japgolly.scalajs.react.component.ScalaForwardRef
import japgolly.scalajs.react.vdom._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.forms.ExternalValue
import lucuma.ui.forms.FormInputEV
import org.scalajs.dom.Window
import react.common.Css
import react.common.EnumValue
import react.common.EnumValueB
import react.common.GenericComponentP
import react.common.GenericComponentP2Render
import react.common.GenericComponentPA
import react.common.GenericComponentPAC
import react.common.GenericComponentPACF
import react.common.GenericComponentPC
import react.common.GenericFnComponentP
import react.common.GenericFnComponentPA
import react.common.GenericFnComponentPAC
import react.common.GenericFnComponentPC
import react.common.ReactRender
import react.common.implicits._

import scala.scalajs.js
import scala.scalajs.js.UndefOr

package object ui {
  implicit class WindowOps(val self: Window) extends AnyVal {
    def canFitTwoPanels: Boolean =
      self.innerWidth <= Constants.TwoPanelCutoff
  }

  implicit class FormInputEVOps[EV[_], A, B](val input: FormInputEV[EV, Option[A]]) extends AnyVal {
    def clearable(implicit ev: ExternalValue[EV], ev3: Eq[A]) =
      input.copy(icon = clearInputIcon[EV, A](input.value))

    // When an icon is added to a FormInputEV, SUI adds extra padding on the right to make
    // space for the icon. However, with some layouts this can cause resizing issues, so this
    // method removes that extra padding. See `clearInputIcon` for more details.
    def clearableNoPadding(implicit ev: ExternalValue[EV], ev3: Eq[A]) = {
      val newClazz: UndefOr[Css] =
        input.clazz.fold(ExploreStyles.ClearableInputPaddingReset)(
          _ |+| ExploreStyles.ClearableInputPaddingReset
        )
      input.copy(icon = clearInputIcon[EV, A](input.value), clazz = newClazz)
    }
  }

  implicit class InputWithUnitsOps[EV[_], A, B](val input: InputWithUnits[EV, Option[A]])
      extends AnyVal {
    def clearable(implicit ev: ExternalValue[EV], ev3: Eq[A]) =
      input.copy(icon = clearInputIcon[EV, A](input.value))

    // When an icon is added to a FormInputEV, SUI adds extra padding on the right to make
    // space for the icon. However, with some layouts this can cause resizing issues, so this
    // method removes that extra padding. See `clearInputIcon` for more details.
    def clearableNoPadding(implicit ev: ExternalValue[EV], ev3: Eq[A]) = {
      val newClazz = input.clazz |+| ExploreStyles.ClearableInputPaddingReset
      input.copy(icon = clearInputIcon[EV, A](input.value), clazz = newClazz)
    }
  }

  // Move to lucuma ui syntax
  type ClassP[P <: js.Object] = GenericComponentP[P]
  given Conversion[ClassP[?], UndefOr[VdomNode]] = _.render.vdomElement
  given Conversion[ClassP[?], VdomNode]          = _.render.vdomElement

  type FnP[P <: js.Object] = GenericFnComponentP[P]
  given Conversion[FnP[?], VdomNode] = _.render

  implicit class GenericComponentPCOps[P <: js.Object, A](val c: GenericComponentPC[P, A])
      extends AnyVal {
    def apply(children: VdomNode*): A = c.withChildren(children)
  }

  given props2Component[Props, S, B, CT[-p, +u] <: CtorType[p, u]]
    : Conversion[ReactRender[Props, CT, Scala.Unmounted[Props, S, B]], VdomElement] =
    _.toUnmounted

  extension [A](c: js.UndefOr[A => Callback])
    def toJs: js.UndefOr[js.Function1[A, Unit]] = c.map(x => (a: A) => x(a).runNow())

}
