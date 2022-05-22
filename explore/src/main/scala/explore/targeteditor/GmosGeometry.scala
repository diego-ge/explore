// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order
import cats.data.NonEmptyMap
import cats.syntax.all._
import explore.model.ScienceMode
import explore.model.ScienceModeBasic
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.GmosSouthFpu
import lucuma.core.enum.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos
import lucuma.core.geom.syntax.shapeexpression._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int._
import lucuma.svgdotjs._
import react.aladin.visualization.svg._
import react.common.implicits._
import react.common.style.Css

/**
 * Test object to produce a gmos geometry. it is for demo purposes only
 */
object GmosGeometry {

  val posAngle: Angle =
    145.deg

  val guideStarOffset: Offset =
    Offset(170543999.µas.p, -24177003.µas.q)

  val offsetPos: Offset =
    Offset(-60.arcsec.p, 60.arcsec.q)

  val fpu: Option[Either[GmosNorthFpu, GmosSouthFpu]] =
    Some(Right(GmosSouthFpu.LongSlit_5_00))

  val port: PortDisposition =
    PortDisposition.Side

  // Move to react common
  implicit val cssOrder: Order[Css] = Order.by(_.htmlClass)

  // Shape to display for a specific mode
  def shapesForMode(posAngle: Angle, mode: Option[ScienceMode]): NonEmptyMap[Css, ShapeExpression] =
    mode match {
      case Some(ScienceMode.GmosNorthLongSlit(ScienceModeBasic.GmosNorthLongSlit(_, _, fpu), _)) =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle),
          (Css("gmos-fpu"), gmos.scienceArea.shapeAt(posAngle, Offset.Zero, fpu.asLeft.some))
        )
      case Some(ScienceMode.GmosSouthLongSlit(ScienceModeBasic.GmosSouthLongSlit(_, _, fpu), _)) =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle),
          (Css("gmos-fpu"), gmos.scienceArea.shapeAt(posAngle, Offset.Zero, fpu.asRight.some))
        )
      case _                                                                                     =>
        NonEmptyMap.of(
          (Css("gmos-science-ccd"), gmos.scienceArea.imaging ⟲ posAngle)
        )
    }

  // Shape to display always
  def commonShapes(posAngle: Angle, extraCss: Css): NonEmptyMap[Css, ShapeExpression] =
    NonEmptyMap.of(
      (Css("gmos-candidates-area") |+| extraCss,
       gmos.probeArm.candidatesAreaAt(posAngle, Offset.Zero)
      )
    )

  // Firefox doesn't properly handle very large coordinates, scale by 1000 at least
  val ScaleFactor = 1000

  val pp: SvgPostProcessor = {
    case p: Polygon   => p.addClass("jts-polygon").addClass("jts")
    case g: Group     => g.addClass("jts-group").addClass("jts")
    case c: Container => c.addClass("jts")
    case a            => a
  }

}
