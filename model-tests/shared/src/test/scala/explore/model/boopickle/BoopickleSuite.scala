// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.boopickle.PicklerTests
import explore.events._
import explore.model.arb.ArbCatalogResults._
import explore.model.boopickle.CatalogPicklers
import explore.model.boopickle.CommonPicklers
import explore.model.boopickle.ItcPicklers
import lucuma.ags.GuideStarCandidate
import lucuma.ags.arb.ArbGuideStarCandidate._
import lucuma.core.math._
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.math.arb.ArbCoordinates._
import lucuma.core.math.arb.ArbDeclination._
import lucuma.core.math.arb.ArbEpoch._
import lucuma.core.math.arb.ArbParallax._
import lucuma.core.math.arb.ArbProperMotion._
import lucuma.core.math.arb.ArbRadialVelocity._
import lucuma.core.math.arb.ArbRightAscension._
import lucuma.core.math.arb.ArbWavelength._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.SiderealTracking
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.arb.ArbConstraintSet._
import lucuma.core.model.arb.ArbElevationRange._
import lucuma.core.model.arb.ArbSiderealTracking._
import lucuma.core.model.arb.ArbUnnormalizedSED._

class BoopickleSuite
    extends munit.DisciplineSuite
    with CatalogPicklers
    with CommonPicklers
    with ItcPicklers
    with EventPicklers {
  checkAll("Pickler[Angle]", PicklerTests[Angle].pickler)
  checkAll("Pickler[HourAngle]", PicklerTests[HourAngle].pickler)
  checkAll("Pickler[RightAscension]", PicklerTests[RightAscension].pickler)
  checkAll("Pickler[NonEmptyString]", PicklerTests[NonEmptyString].pickler)
  checkAll("Pickler[PosInt]", PicklerTests[PosInt].pickler)
  checkAll("Pickler[Declination]", PicklerTests[Declination].pickler)
  checkAll("Pickler[Coordinates]", PicklerTests[Coordinates].pickler)
  checkAll("Pickler[Epoch]", PicklerTests[Epoch].pickler)
  checkAll("Pickler[ProperMotion]", PicklerTests[ProperMotion].pickler)
  checkAll("Pickler[RadialVelocity]", PicklerTests[RadialVelocity].pickler)
  checkAll("Pickler[Parallax]", PicklerTests[Parallax].pickler)
  checkAll("Pickler[SiderealTracking]", PicklerTests[SiderealTracking].pickler)
  checkAll("Pickler[GuideStarCandidate]", PicklerTests[GuideStarCandidate].pickler)
  checkAll("Pickler[ElevationRange]", PicklerTests[ElevationRange].pickler)
  checkAll("Pickler[ConstraintSet]", PicklerTests[ConstraintSet].pickler)
  checkAll("Pickler[Wavelengtth]", PicklerTests[Wavelength].pickler)

  checkAll("Pickler[UnnormalizedSED]", PicklerTests[UnnormalizedSED].pickler)

  // Events
  checkAll("Pickler[CatalogResults]", PicklerTests[CatalogResults].pickler)
}
