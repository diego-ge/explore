// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.enums.TimeDisplay
import explore.syntax.ui.*
import explore.syntax.ui.given
import gpp.highcharts.highchartsStrings.line
import gpp.highcharts.mod.XAxisLabelsOptions
import gpp.highcharts.mod._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.ObservingNight
import lucuma.core.util.Enumerated
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.highcharts.Chart
import react.moon.MoonPhase
import react.resizeDetector.hooks._

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.collection.immutable.HashSet
import scala.deriving.Mirror
import scala.scalajs.js

import js.JSConverters._

final case class ElevationPlotNight(
  site:        Site,
  coords:      Coordinates,
  date:        LocalDate,
  timeDisplay: TimeDisplay
) extends ReactFnProps[ElevationPlotNight](ElevationPlotNight.component)

object ElevationPlotNight {
  type Props = ElevationPlotNight

  private val PlotEvery: Duration   = Duration.ofMinutes(1)
  private val MillisPerHour: Double = 60 * 60 * 1000

  @js.native
  protected trait PointOptionsWithAirmass extends PointOptionsObject {
    var airmass: Double
  }

  @js.native
  protected trait ElevationPointWithAirmass extends Point {
    var airmass: Double
  }

  protected implicit class PointOptionsWithAirmassOps(val x: PointOptionsWithAirmass)
      extends AnyVal     {
    def setAirMass(value: Double): PointOptionsWithAirmass = {
      x.airmass = value
      x
    }
  }

  protected case class SeriesData(
    targetAltitude:   List[Chart.Data],
    skyBrightness:    List[Chart.Data],
    parallacticAngle: List[Chart.Data],
    moonAltitude:     List[Chart.Data]
  )

  sealed abstract class ElevationSeries(
    val name:  String,
    val yAxis: Int,
    val data:  SeriesData => List[Chart.Data]
  ) extends Product
      with Serializable
  object ElevationSeries {
    case object Elevation        extends ElevationSeries("Elevation", 0, _.targetAltitude)
    case object ParallacticAngle extends ElevationSeries("Parallactic Angle", 1, _.parallacticAngle)
    case object SkyBrightness    extends ElevationSeries("Sky Brightness", 2, _.skyBrightness)
    case object LunarElevation   extends ElevationSeries("Lunar Elevation", 0, _.moonAltitude)

    def tag(a: ElevationSeries) = a.name

    implicit val ElevationSeriesEnumerated: Enumerated[ElevationSeries] =
      Enumerated.of(Elevation, ParallacticAngle, SkyBrightness, LunarElevation)
  }

  private val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")

  private def formatAngle(degs: Double): String = {
    val dms     = Angle.DMS(Angle.fromDoubleDegrees(degs))
    val degrees = if (dms.degrees > 180) s"-${360 - dms.degrees}" else dms.degrees.toString
    val minutes = "%02d".format(dms.arcminutes)
    val seconds = "%02d".format(dms.arcseconds)
    s"$degrees°$minutes′$seconds″"
  }

  private val skyBrightnessPercentileLines = {
    def plotLine(id: String, value: Double) =
      YAxisPlotLinesOptions()
        .setId(s"sky-brightness-$id")
        .setValue(value)
        .setClassName("plot-sky-brightness-percentile")
        .setDashStyle(DashStyleValue.Dot)
        .setZIndex(1)

    List(
      plotLine("20", 21.37),
      plotLine("50", 20.78),
      plotLine("80", 19.61)
    )
  }

  private val skyBrightnessPercentileBands = {
    def plotBand(id: String, label: String, from: Double, to: Double) =
      YAxisPlotBandsOptions()
        .setId(s"sky-brightness-$id")
        .setLabel(
          YAxisPlotBandsLabelOptions()
            .setText(s"<span class='plot-sky-brightness-band-label'>$label</span>")
            .setVerticalAlign(VerticalAlignValue.middle)
            .setAlign(AlignValue.right)
        )
        .setFrom(from)
        .setTo(to)
        .setClassName("plot-sky-brightness-band")
        .setZIndex(1)

    List(
      plotBand("darketst", "Darkest", 21.37, 22),
      plotBand("dark", "Dark", 20.78, 21.37),
      plotBand("gray", "Gray", 19.61, 20.78),
      plotBand("bright", "Bright", 17, 19.61)
    )
  }

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(HashSet.from(Enumerated[ElevationSeries].all))
      .useResizeDetector()
      .render { (props, shownSeries, resize) =>
        def showSeriesCB(series: ElevationSeries, chart: Chart_): Callback =
          shownSeries.modState(_ + series) >>
            Callback {
              skyBrightnessPercentileLines.foreach(line => chart.yAxis(2).addPlotLine(line))
              skyBrightnessPercentileBands.foreach(band => chart.yAxis(2).addPlotBand(band))
            }
              .when(series === ElevationSeries.SkyBrightness)
              .void

        def hideSeriesCB(series: ElevationSeries, chart: Chart_): Callback =
          shownSeries.modState(_ - series) >>
            Callback {
              skyBrightnessPercentileLines
                .flatMap(_.id.toList)
                .foreach(id => chart.yAxis(2).removePlotLine(id))
              skyBrightnessPercentileBands
                .flatMap(_.id.toList)
                .foreach(id => chart.yAxis(2).removePlotBand(id))
            }
              .when(series === ElevationSeries.SkyBrightness)
              .void

        val observingNight  = ObservingNight.fromSiteAndLocalDate(props.site, props.date)
        val tbOfficialNight = observingNight.twilightBoundedUnsafe(TwilightType.Official)
        val tbNauticalNight = observingNight.twilightBoundedUnsafe(TwilightType.Nautical)

        val start          = tbOfficialNight.start
        val end            = tbOfficialNight.end
        val skyCalcResults =
          SkyCalc.forInterval(props.site, start, end, PlotEvery, _ => props.coords)
        val series         = skyCalcResults
          .map { case (instant, results) =>
            val millisSinceEpoch = instant.toEpochMilli.toDouble

            def point(value: Double): Chart.Data =
              PointOptionsObject(js.undefined)
                .setX(millisSinceEpoch)
                .setY(value)

            def pointWithAirmass(value: Double, airmass: Double): Chart.Data =
              point(value).asInstanceOf[PointOptionsWithAirmass].setAirMass(airmass)

            (pointWithAirmass(results.altitude.toAngle.toSignedDoubleDegrees, results.airmass),
             point(results.totalSkyBrightness),
             point(results.parallacticAngle.toSignedDoubleDegrees),
             point(results.lunarElevation.toAngle.toSignedDoubleDegrees)
            )
          }

        val seriesData = summon[Mirror.Of[SeriesData]].fromProduct(unzip4(series))

        def timezoneInstantFormat(instant: Instant, zoneId: ZoneId): String =
          ZonedDateTime
            .ofInstant(instant, zoneId)
            .format(dateTimeFormatter)

        def instantFormat(instant: Instant): String =
          props.timeDisplay match {
            case TimeDisplay.Site     => timezoneInstantFormat(instant, props.site.timezone)
            case TimeDisplay.UT       => timezoneInstantFormat(instant, ZoneOffset.UTC)
            case TimeDisplay.Sidereal =>
              val skycalc = ImprovedSkyCalc(props.site.place)
              val sid     = skycalc.getSiderealTime(instant)
              val hours   = sid.toInt
              val minutes = Math.round((sid % 1) * 60).toInt
              f"$hours%02d:$minutes%02d"
          }

        def timeFormat(value: Double): String =
          instantFormat(Instant.ofEpochMilli(value.toLong))

        val timeDisplay: String =
          props.timeDisplay match {
            case TimeDisplay.Site     => props.site.timezone.getId
            case TimeDisplay.UT       => "UTC"
            case TimeDisplay.Sidereal => "Site Sidereal"
          }

        val tickFormatter: AxisLabelsFormatterCallbackFunction =
          (
            labelValue: AxisLabelsFormatterContextObject,
            _:          AxisLabelsFormatterContextObject
          ) => timeFormat(labelValue.value.asInstanceOf[Double])

        val tooltipFormatter: TooltipFormatterCallbackFunction = {
          (ctx: TooltipFormatterContextObject, _: Tooltip) =>
            val x     = ctx.x match
              case x: Double => x
              case x: String => x.toDouble
              case _         => 0.0
            val y     = ctx.y match
              case y: Double => y
              case y: String => y.toDouble
              case _         => 0.0
            val time  = timeFormat(x)
            val value = ctx.series.index match {
              case 0 =>                      // Target elevation with airmass
                formatAngle(y) + s"<br/>Airmass: ${"%.3f".format(ctx.point.asInstanceOf[ElevationPointWithAirmass].airmass)}"
              case 2 => "%.2f".format(ctx.y) // Sky Brightness
              case _ => formatAngle(y)       // Other elevations
            }
            s"<strong>$time ($timeDisplay)</strong><br/>${ctx.series.name}: $value"
        }

        val sunset  = instantFormat(tbNauticalNight.start)
        val sunrise = instantFormat(tbNauticalNight.end)

        val targetBelowHorizon =
          ElevationSeries.Elevation
            .data(seriesData)
            .forall(_.asInstanceOf[PointOptionsObject].y.forall(_.asInstanceOf[Double] <= 0))

        val options = Options()
          .setChart(
            ChartOptions()
              .setHeight(resize.height.getOrElse(1).toDouble)
              .setStyledMode(true)
              .setAlignTicks(false)
          )
          .setTitle(
            TitleOptions().setText(
              s"Sunset ${observingNight.toLocalDate.minusDays(1)} ➟ Sunrise ${observingNight.toLocalDate} "
            )
          )
          .setCredits(CreditsOptions().setEnabled(false))
          .setTooltip(TooltipOptions().setFormatter(tooltipFormatter))
          .setXAxis(
            XAxisOptions()
              .setType(AxisTypeValue.datetime)
              .setLabels(XAxisLabelsOptions().setFormatter(tickFormatter))
              .setTickInterval(MillisPerHour)
              .setMinorTickInterval(MillisPerHour / 2)
              .setTitle(
                if (targetBelowHorizon) XAxisTitleOptions().setText("Target is below horizon")
                else XAxisTitleOptions()
              )
              .setPlotBands(
                List(
                  XAxisPlotBandsOptions()
                    .setFrom(tbNauticalNight.start.toEpochMilli.toDouble)
                    .setTo(tbNauticalNight.end.toEpochMilli.toDouble)
                    .setClassName("plot-band-twilight-nautical")
                    // We need z-index > 0 to display over grid. But not too high, or it will display over tooltips.
                    .setZIndex(1)
                    .setLabel(
                      XAxisPlotBandsLabelOptions()
                        .setText(s"  Evening 12° - Twilight: $sunset")
                        .setRotation(270)
                        .setAlign(AlignValue.left)
                        .setTextAlign(AlignValue.center)
                        .setVerticalAlign(VerticalAlignValue.middle)
                    ),
                  XAxisPlotBandsOptions() // Empty band, just to draw the label
                    .setFrom(tbNauticalNight.end.toEpochMilli.toDouble)
                    .setTo(tbNauticalNight.end.toEpochMilli.toDouble)
                    .setClassName("plot-band-twilight-nautical-end")
                    .setZIndex(1)
                    .setLabel(
                      XAxisPlotBandsLabelOptions()
                        .setText(s"  Morning 12° - Twilight: $sunrise")
                        .setRotation(270)
                        .setAlign(AlignValue.left)
                        .setTextAlign(AlignValue.center)
                        .setVerticalAlign(VerticalAlignValue.middle)
                    )
                ).toJSArray
              )
          )
          .setYAxis(
            List(
              YAxisOptions()
                .setTitle(YAxisTitleOptions().setText("Elevation"))
                .setAllowDecimals(false)
                .setMin(0)
                .setMax(90)
                .setTickInterval(10)
                .setMinorTickInterval(5)
                .setLabels(YAxisLabelsOptions().setFormat("{value}°")),
              YAxisOptions()
                .setOpposite(true)
                .setTitle(YAxisTitleOptions().setText("Parallactic angle"))
                .setMin(-180)
                .setMax(180)
                .setTickInterval(60)
                .setClassName("plot-axis-parallactic-angle")
                .setShowEmpty(false)
                .setLabels(YAxisLabelsOptions().setFormat("{value}°")),
              YAxisOptions()
                .setOpposite(true)
                .setTitle(YAxisTitleOptions().setText("Sky Brightness [V] (mag/arcsec²)"))
                .setMin(17)
                .setMax(22)
                .setReversed(true)
                .setTickInterval(1)
                .setClassName("plot-axis-sky-brightness")
                .setShowEmpty(false)
                .setLabels(YAxisLabelsOptions().setFormat("{value}"))
                .setPlotLines(
                  if (shownSeries.value.contains(ElevationSeries.SkyBrightness))
                    skyBrightnessPercentileLines.toJSArray
                  else
                    js.Array()
                )
                .setPlotBands(
                  if (shownSeries.value.contains(ElevationSeries.SkyBrightness))
                    skyBrightnessPercentileBands.toJSArray
                  else
                    js.Array()
                )
            ).toJSArray
          )
          .setPlotOptions(
            PlotOptions()
              .setSeries(
                PlotSeriesOptions()
                  .setLineWidth(4)
                  .setMarker(PointMarkerOptionsObject().setEnabled(false))
                  .setStates(
                    SeriesStatesOptionsObject()
                      .setHover(SeriesStatesHoverOptionsObject().setEnabled(false))
                  )
              )
          )
          .setSeries(
            Enumerated[ElevationSeries].all
              .map(series =>
                SeriesLineOptions((), (), line)
                  .setName(series.name)
                  .setYAxis(series.yAxis)
                  .setData(series.data(seriesData).toJSArray)
                  .setVisible(shownSeries.value.contains(series))
                  .setEvents(
                    SeriesEventsOptionsObject()
                      .setHide((s, _) => hideSeriesCB(series, s.chart).runNow())
                      .setShow((s, _) => showSeriesCB(series, s.chart).runNow())
                  )
              )
              .map(_.asInstanceOf[SeriesOptionsType])
              .toJSArray
          )

        val (moonPhase, moonIllum) = skyCalcResults(
          skyCalcResults.length / 2
        ).bimap(MoonCalc.approxPhase, _.lunarIlluminatedFraction.toDouble)

        <.div(
          // Include the size in the key
          Chart(options).withKey(s"$props-$resize").when(resize.height.isDefined),
          <.div(ExploreStyles.MoonPhase)(
            <.span(
              MoonPhase(phase = moonPhase,
                        size = 20,
                        border = "1px solid black",
                        darkColor = "#303030"
              ),
              <.small("%1.0f%%".format(moonIllum * 100))
            )
          )
        )
          .withRef(resize.ref)
      }
}
