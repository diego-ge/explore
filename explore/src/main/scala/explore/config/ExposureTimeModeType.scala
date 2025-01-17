// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.Reusability
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*

sealed trait ExposureTimeModeType extends Product with Serializable {
  def label: NonEmptyString
}

object ExposureTimeModeType {
  case object SignalToNoise extends ExposureTimeModeType { override val label = "S/N".refined   }
  case object FixedExposure extends ExposureTimeModeType { override val label = "Fixed".refined }

  def fromExposureTimeMode(etm: ExposureTimeMode): ExposureTimeModeType = etm match {
    case ExposureTimeMode.SignalToNoise(_)    => SignalToNoise
    case ExposureTimeMode.FixedExposure(_, _) => FixedExposure
  }

  implicit val enumExposureTimeModeType: Enumerated[ExposureTimeModeType] =
    Enumerated.of(SignalToNoise, FixedExposure)

  implicit val displayExposureTimeModeType: Display[ExposureTimeModeType] =
    Display.byShortName(_.label.value)

  implicit val reuseExposureTimeModeType: Reusability[ExposureTimeModeType] = Reusability.byEq
}
