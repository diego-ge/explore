// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse.Reuse
import crystal.react.reuse._
import explore.Icons
import explore.Resources
import explore.components.About
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ExploreLocalPreferences
import explore.model.ExploreLocalPreferences._
import explore.model.ModelUndoStacks
import explore.model.enums.ExecutionEnvironment
import explore.model.enums.Theme
import explore.programs.ProgramsPopup
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.vdom.html_<^._
import log4cats.loglevel.LogLevelLogger
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom
import org.scalajs.dom.window
import react.common.ReactFnProps
import react.semanticui.collections.menu._
import react.semanticui.elements.image.Image
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.Dropdown
import react.semanticui.modules.dropdown.DropdownDivider
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown.DropdownMenu
import react.semanticui.shorthand._
import react.semanticui.views.item.Item
import typings.loglevel.mod.LogLevelDesc

final case class TopBar(
  user:        User,
  programId:   Option[Program.Id],
  preferences: ExploreLocalPreferences,
  undoStacks:  View[ModelUndoStacks[IO]],
  onLogout:    IO[Unit]
) extends ReactFnProps[TopBar](TopBar.component)

object TopBar {
  type Props = TopBar

  private def bodyClasses: dom.DOMTokenList = dom.document.body.classList

  private def currentTheme: Theme =
    if (bodyClasses.contains(Theme.Light.clazz.htmlClass))
      Theme.Light
    else
      Theme.Dark

  private def flipTheme(theme: Theme): Theme =
    if (theme === Theme.Dark) Theme.Light else Theme.Dark

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(false)        // isProgramsOpen
      .useState(currentTheme) // theme
      .render { (props, isProgramsOpen, theme) =>
        AppCtx.using { implicit appCtx =>
          val role = props.user.role

          def logout: IO[Unit] = appCtx.sso.logout >> props.onLogout

          val level = props.preferences.level

          def setLogLevel(l: LogLevelDesc): Callback =
            (ExploreLocalPreferences
              .storePreferences[IO](
                props.preferences.copy(level = l)
              ) *> IO(window.location.reload(false))).runAsync

          <.div(
            ExploreStyles.MainHeader,
            Menu(
              attached = MenuAttached.Top,
              borderless = true,
              tabular = MenuTabular.Right
            )(
              MenuItem(
                <.span(
                  ExploreStyles.MainTitle,
                  "Explore"
                )
              ),
              Item(
                ExploreStyles.MainUserName,
                props.user.displayName
              ),
              ConnectionsStatus(),
              MenuMenu(position = MenuMenuPosition.Right, clazz = ExploreStyles.MainMenu)(
                Dropdown(
                  item = true,
                  simple = true,
                  compact = true,
                  icon = Icons.Bars,
                  open = false,
                  clazz = ExploreStyles.MainMenuDropdown
                )(
                  DropdownMenu(
                    About(
                      Reuse.always(
                        DropdownItem(text = "About Explore", icon = Icons.Info.fixedWidth())
                      )
                    ),
                    DropdownItem(
                      text = "Manage Programs",
                      icon = Icons.ListCheck.fixedWidth(),
                      onClick = isProgramsOpen.setState(true)
                    ),
                    TagMod.when(isProgramsOpen.value)(
                      ProgramsPopup(
                        props.programId,
                        props.undoStacks,
                        isProgramsOpen.setState(false).some.reuseAlways
                      )
                    ),
                    DropdownDivider(),
                    DropdownItem(
                      onClick = appCtx.sso.switchToORCID.runAsync
                    )(
                      <.div(ExploreStyles.OrcidMenu)(
                        Image(clazz = ExploreStyles.OrcidIconMenu, src = Resources.OrcidLogo),
                        <.span(^.cls := "text", "Login with ORCID")
                      )
                    ).when(role === GuestRole),
                    DropdownItem(
                      text = "Logout",
                      icon = Icons.Logout.fixedWidth(),
                      onClick = logout.runAsync
                    ),
                    DropdownItem()(
                      Icons.BarCodeRead.fixedWidth(),
                      "Log Level",
                      DropdownMenu(
                        DropdownItem(onClick = setLogLevel(LogLevelDesc.INFO))(
                          Checkbox(label = "Info", checked = level =!= LogLevelDesc.DEBUG)
                        ),
                        DropdownItem(onClick = setLogLevel(LogLevelDesc.DEBUG))(
                          Checkbox(label = "Debug", checked = level === LogLevelLogger.Level.DEBUG)
                        )
                      )
                    ).when(appCtx.environment =!= ExecutionEnvironment.Production),
                    DropdownItem(
                      onClick = utils.setupScheme[CallbackTo](
                        if (theme.value === Theme.Dark) Theme.Light else Theme.Dark
                      ) *> theme.modState(flipTheme)
                    )(
                      Checkbox(label = "Dark/Light", checked = currentTheme === Theme.Dark)
                    )
                      .when(appCtx.environment === ExecutionEnvironment.Development),
                    DropdownItem(
                      text = "Toggle Reusability",
                      icon = Icons.CrystalBall.fixedWidth(),
                      onClick = utils.toggleReusabilityOverlay[CallbackTo]()
                    )
                      .when(appCtx.environment === ExecutionEnvironment.Development)
                  )
                )
              )
            )
          )
        }
      }

}
