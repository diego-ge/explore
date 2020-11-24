// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import scala.collection.immutable.SortedSet

import cats.Order._
import cats.kernel.Eq
import explore.model.enum.AppTab
import lucuma.core.data.EnumZipper
import lucuma.core.model.Target
import lucuma.core.model.Target.Id._
import monocle.Lens
import monocle.macros.Lenses
import java.time.Instant

@Lenses
case class RootModel(
  vault:             UserVault,
  tabs:              EnumZipper[AppTab],
  focused:           Option[Focused] = None,
  expandedTargetIds: SortedSet[Target.Id] = SortedSet.empty
)

object RootModel {
  implicit val eqRootModel: Eq[RootModel] = Eq.by(m => (m.vault, m.tabs, m.focused))

  lazy val expiration: Lens[RootModel, Instant] = vault ^|-> UserVault.expiration
}
