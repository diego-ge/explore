// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.syntax

import explore.model.enum.PosAngleOptions
import explore.model.PosAngle
import lucuma.core.math.Angle

object all {
  implicit class PosAngleOptionsOps(val pa: PosAngleOptions) extends AnyVal {
    def toPosAngle(a: Angle): PosAngle = pa match {
      case PosAngleOptions.Fixed               => PosAngle.Fixed(a)
      case PosAngleOptions.AllowFlip           => PosAngle.AllowFlip(a)
      case PosAngleOptions.AverageParallactic  => PosAngle.AverageParallactic(a)
      case PosAngleOptions.ParallacticOverride => PosAngle.ParallacticOverride(a)
    }
  }

  implicit class PosAngleOps(val pa: PosAngle) extends AnyVal {
    def toPosAngleOption: PosAngleOptions = pa match {
      case PosAngle.Fixed(_)               => PosAngleOptions.Fixed
      case PosAngle.AllowFlip(_)           => PosAngleOptions.AllowFlip
      case PosAngle.AverageParallactic(_)  => PosAngleOptions.AverageParallactic
      case PosAngle.ParallacticOverride(_) => PosAngleOptions.ParallacticOverride
    }
  }
}
