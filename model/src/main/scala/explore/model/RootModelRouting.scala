// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.implicits._
import explore.model.Focused.FocusedObs
import explore.model.Focused.FocusedTarget
import explore.model.Page._
import explore.model.enum.AppTab
import monocle.Lens

object RootModelRouting {

  protected def getPage(model: RootModel): Page =
    model.tabs.focus match {
      case AppTab.Overview       => HomePage
      case AppTab.Observations   =>
        RootModel.focused
          .get(model)
          .collect { case FocusedObs(obsId) => ObsPage(obsId) }
          .getOrElse(ObservationsBasePage)
      case AppTab.Targets        =>
        RootModel.focused
          .get(model)
          .collect {
            case FocusedObs(obsId)       => TargetsObsPage(obsId)
            case FocusedTarget(targetId) => TargetPage(targetId)
          }
          .getOrElse(TargetsBasePage)
      case AppTab.Configurations => ConfigurationsPage
      case AppTab.Constraints    => ConstraintsPage
    }

  protected def setTab(tab: AppTab): RootModel => RootModel =
    RootModel.tabs.modify(_.withFocus(tab))

  protected def setPage(page: Page): RootModel => RootModel =
    page match {
      case ObservationsBasePage  =>
        setTab(AppTab.Observations) >>> RootModel.focused.set(none)
      case ObsPage(obsId)        =>
        setTab(AppTab.Observations) >>> RootModel.focused.set(FocusedObs(obsId).some)
      case TargetsBasePage       =>
        setTab(AppTab.Targets) >>> RootModel.focused.set(none)
      case TargetPage(targetId)  =>
        setTab(AppTab.Targets) >>> RootModel.focused.set(FocusedTarget(targetId).some)
      case TargetsObsPage(obsId) =>
        setTab(AppTab.Targets) >>> RootModel.focused.set(FocusedObs(obsId).some)
      case ConstraintsPage       =>
        setTab(AppTab.Constraints)
      case ConfigurationsPage    =>
        setTab(AppTab.Configurations)
      case HomePage              =>
        setTab(AppTab.Overview)
    }

  val lens: Lens[RootModel, Page] =
    Lens(getPage)(setPage)
}