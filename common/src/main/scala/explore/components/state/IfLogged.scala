// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import clue.data.Input
import crystal.react.View
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.components.UserSelectionForm
import explore.implicits._
import explore.model.RootModel
import explore.model.UserVault
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.UserPreferencesQueriesGQL._
import react.common.ReactFnProps

final case class IfLogged(view: View[RootModel])(
  val render:                   (UserVault, IO[Unit]) => VdomNode
) extends ReactFnProps[IfLogged](IfLogged.component)

object IfLogged {
  type Props = IfLogged

  // Creates a "profile" for user preferences.
  private def createUserPrefs(vault: UserVault)(implicit ctx: AppContextIO): IO[Unit] =
    UserInsertMutation.execute(Input(vault.user.id.toString)).start.void

  private val component =
    ScalaFnComponent[IfLogged] { p =>
      AppCtx.using { implicit ctx =>
        val vaultView   = p.view.zoom(RootModel.vault)
        val messageView = p.view.zoom(RootModel.userSelectionMessage)

        val vaultSet   = vaultView.set.reuseAlways
        val messageSet = messageView.set.compose((s: NonEmptyString) => s.some).reuseAlways

        vaultView.get.fold[VdomElement](
          UserSelectionForm(vaultView, messageView)
        ) { vault =>
          React.Fragment(
            SSOManager(vault.expiration, vaultSet, messageSet),
            ConnectionManager(vault.token, onConnect = vault.curryReusing.in(createUserPrefs _))(
              LogoutTracker(vaultSet, messageSet)(p.render(vault, _))
            )
          )
        }
      }
    }
}
