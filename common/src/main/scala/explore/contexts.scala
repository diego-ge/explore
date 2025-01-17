// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import crystal.react.Ctx
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string._
import explore.model.Help
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.Reusability._
import monocle.Focus
import org.http4s.Uri

object AppCtx extends Ctx[CallbackTo, AppContextIO]

case class HelpContext(
  rawUrl:        Uri,
  editUrl:       Uri,
  user:          String,
  project:       String,
  displayedHelp: Option[Help.Id] = none
)

object HelpContext {
  val displayedHelp = Focus[HelpContext](_.displayedHelp)

  implicit val helpIdReuse: Reusability[Help.Id]          = Reusability.by(_.value)
  implicit val uriReuse: Reusability[Uri]                 = Reusability.by(_.toString)
  implicit val helpContextReuse: Reusability[HelpContext] =
    Reusability.by(x => (x.rawUrl, x.editUrl, x.user, x.project, x.displayedHelp))
}

object HelpCtx extends Ctx[CallbackTo, HelpContext]
