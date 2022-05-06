// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.syntax.all._
import coulomb.Quantity
import crystal.react._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.ObsQueries._
import explore.common.ScienceQueries._
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.ITCTarget
import explore.model.ImagingConfigurationOptions
import explore.model.ObsConfiguration
import explore.model.PosAngle
import explore.model.SpectroscopyConfigurationOptions
import explore.model.TruncatedPA
import explore.model.display._
import explore.model.enum.PosAngleOptions
import explore.model.formats.angleTruncatedPASplitEpi
import explore.model.reusability._
import explore.model.syntax.all._
import explore.targeteditor.InputWithUnits
import explore.undo.UndoContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.util.syntax._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ScienceMode
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.optics.syntax.lens._
import lucuma.core.syntax.string._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.Iso
import monocle.Lens
import react.common._
import react.datepicker._
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

import scalajs.js
import scalajs.js.|

final case class ConfigurationPanel(
  obsId:            Observation.Id,
  obsConf:          ReuseView[ObsConfiguration],
  scienceDataUndo:  Reuse[UndoContext[ScienceData]],
  constraints:      ConstraintSet,
  itcTargets:       List[ITCTarget],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel {
  type Props = ConfigurationPanel

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  implicit val ldtReuse: Reusability[LocalDateTime] =
    Reusability.by(_.toEpochSecond(ZoneOffset.UTC))

  val dataIso: Iso[SpectroscopyRequirementsData, SpectroscopyConfigurationOptions] =
    Iso[SpectroscopyRequirementsData, SpectroscopyConfigurationOptions] { s =>
      def wavelengthToMicro(w: Wavelength) = w.micrometer.toValue[BigDecimal]

      val op = for {
        _ <- SpectroscopyConfigurationOptions.wavelengthQ         := s.wavelength.map(wavelengthToMicro)
        _ <- SpectroscopyConfigurationOptions.resolution          := s.resolution
        _ <- SpectroscopyConfigurationOptions.signalToNoise       := s.signalToNoise
        _ <- SpectroscopyConfigurationOptions.signalToNoiseAtQ    := s.signalToNoiseAt.map(
               wavelengthToMicro
             )
        _ <- SpectroscopyConfigurationOptions.wavelengthCoverageQ := s.wavelengthCoverage.map(
               wavelengthToMicro
             )
        _ <- SpectroscopyConfigurationOptions.focalPlane          := s.focalPlane
        _ <- SpectroscopyConfigurationOptions.focalPlaneAngle     := s.focalPlaneAngle
        _ <- SpectroscopyConfigurationOptions.capabilities        := s.capabilities
      } yield ()
      op.runS(SpectroscopyConfigurationOptions.Default).value
    } { s =>
      def microToWavelength(m: Quantity[BigDecimal, Micrometer]) =
        Wavelength.decimalMicrometers.getOption(m.value)

      val op = for {
        _ <- SpectroscopyRequirementsData.wavelength         := s.wavelengthQ.flatMap(microToWavelength)
        _ <- SpectroscopyRequirementsData.resolution         := s.resolution
        _ <- SpectroscopyRequirementsData.signalToNoise      := s.signalToNoise
        _ <- SpectroscopyRequirementsData.signalToNoiseAt    := s.signalToNoiseAtQ.flatMap(
               microToWavelength
             )
        _ <- SpectroscopyRequirementsData.wavelengthCoverage := s.wavelengthCoverageQ.flatMap(
               microToWavelength
             )
        _ <- SpectroscopyRequirementsData.focalPlane         := s.focalPlane
        _ <- SpectroscopyRequirementsData.focalPlaneAngle    := s.focalPlaneAngle
        _ <- SpectroscopyRequirementsData.capabilities       := s.capabilities
      } yield ()
      op.runS(SpectroscopyRequirementsData()).value
    }

  // Input for an angle in degrees with up to 2 decimals
  private val truncatedPAAngle = ValidFormatInput[TruncatedPA](
    s => {
      val ota = s.parseDoubleOption
        .map(Angle.fromDoubleDegrees)
        .map(TruncatedPA(_))
      Validated.fromOption(ota, NonEmptyChain("Invalid Position Angle"))
    },
    pa => f"${pa.angle.toDoubleDegrees}%.2f"
  )

  /**
   * Used to convert pos angle and an enumeration for a UI selector It is unsafe as the angle is
   * lost for Average Parallictic and Unconstrained
   */
  private val unsafePosOptionsLens: Lens[PosAngle, PosAngleOptions] =
    Lens[PosAngle, PosAngleOptions](_.toPosAngleOption)((a: PosAngleOptions) =>
      (
        (b: PosAngle) =>
          a.toPosAngle(b match {
            case PosAngle.Fixed(a)               => a
            case PosAngle.AllowFlip(a)           => a
            case PosAngle.AverageParallactic     => Angle.Angle0
            case PosAngle.ParallacticOverride(a) => a
            case PosAngle.Unconstrained          => Angle.Angle0
          })
      )
    )

  // TODO Move these to react-datetime
  implicit class InstantOps(val instant: Instant) extends AnyVal {
    // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
    // See https://github.com/Hacker0x01/react-datepicker/issues/1787
    def toDatePickerJsDate: js.Date =
      new js.Date(instant.toEpochMilli.toDouble + (new js.Date()).getTimezoneOffset() * 60000)
  }

  object InstantBuilder {
    // DatePicker only works in local timezone, so we trick it by adding the timezone offset.
    // See https://github.com/Hacker0x01/react-datepicker/issues/1787
    def fromDatePickerJsDate(jsDate: js.Date): Instant =
      Instant.ofEpochMilli((jsDate.getTime() - jsDate.getTimezoneOffset() * 60000).toLong)
  }

  implicit class JSUndefOrNullOrTuple2DateTimeOps[A](
    val value: js.UndefOr[DateOrRange]
  ) extends AnyVal {
    def toEitherOpt2: Option[Either[(A, A), A]] =
      value.toOption
        .flatMap(valueOrNull => Option(valueOrNull.asInstanceOf[A | js.Tuple2[A, A]]))
        .map { valueOrTuple =>
          if (js.Array.isArray(valueOrTuple))
            Left(valueOrTuple.asInstanceOf[js.Tuple2[A, A]])
          else
            Right(valueOrTuple.asInstanceOf[A])
        }

    def fromDatePickerToInstantEitherOpt(implicit
      ev: A <:< js.Date
    ): Option[Either[(Instant, Instant), Instant]] =
      value.toEitherOpt.map { (e: Either[(js.Date, js.Date), js.Date]) =>
        e match {
          case Left((d1, d2)) =>
            Left((InstantBuilder.fromDatePickerJsDate(d1), InstantBuilder.fromDatePickerJsDate(d2)))
          case Right(d)       =>
            Right(InstantBuilder.fromDatePickerJsDate(d))
        }
      }.widen

    def fromDatePickerToInstantOpt(implicit ev: A <:< js.Date): Option[Instant] =
      fromDatePickerToInstantEitherOpt.flatMap(_.toOption)
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewWithReuse[ScienceMode](ScienceMode.Spectroscopy)
      .useStateViewWithReuse[ImagingConfigurationOptions](ImagingConfigurationOptions.Default)
      .renderWithReuse { (props, mode, imaging) =>
        implicit val ctx: AppContextIO = props.ctx
        val requirementsCtx            = props.scienceDataUndo.map(_.zoom(ScienceData.requirements))

        val requirementsViewSet = requirementsCtx.map(UndoView(props.obsId, _))

        val isSpectroscopy = mode.get === ScienceMode.Spectroscopy

        val spectroscopy = requirementsViewSet.map(
          _(
            ScienceRequirementsData.spectroscopy,
            UpdateScienceRequirements.spectroscopyRequirements
          )
        )

        val modeView = props.scienceDataUndo.map(
          _.undoableView(ScienceData.mode)
            .withOnMod(conf => setScienceMode(props.obsId, conf).runAsync)
        )

        val posAngleOptionsView =
          props.obsConf.zoom(ObsConfiguration.posAngle.andThen(unsafePosOptionsLens))

        val fixedView = props.obsConf
          .zoom(ObsConfiguration.posAngle)
          .zoom(PosAngle.fixedAnglePrism)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        val allowedFlipView = props.obsConf
          .zoom(ObsConfiguration.posAngle)
          .zoom(PosAngle.allowFlipAnglePrism)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        val parallacticOverrideView = props.obsConf
          .zoom(ObsConfiguration.posAngle)
          .zoom(PosAngle.parallacticOverrideAnglePrism)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        val obsInstant = props.obsConf.zoom(ObsConfiguration.obsInstant)

        def posAngleEditor(pa: ReuseView[TruncatedPA]) =
          <.div(
            ExploreStyles.SignalToNoiseAt,
            InputWithUnits[ReuseView, TruncatedPA](
              id = "pos-angle-value",
              clazz = Css.Empty,
              value = pa,
              units = "° E of N",
              validFormat = truncatedPAAngle,
              changeAuditor = ChangeAuditor.accept.decimal(2)
            )
          )

        <.div(
          ExploreStyles.ConfigurationGrid,
          props.renderInTitle(
            <.span(ExploreStyles.TitleUndoButtons)(UndoButtons(props.scienceDataUndo))
          ),
          Form(size = Small)(
            ExploreStyles.Compact,
            ExploreStyles.ObsConfigurationForm
          )(
            <.div(
              ExploreStyles.ObsConfigurationObsPA,
              <.label("Position Angle", HelpIcon("configuration/positionangle.md")),
              EnumViewSelect[ReuseView, PosAngleOptions](
                id = "pos-angle-alternative",
                value = posAngleOptionsView
              ),
              fixedView.mapValue(posAngleEditor),
              allowedFlipView.mapValue(posAngleEditor),
              parallacticOverrideView.mapValue(posAngleEditor)
            ),
            <.div(
              ExploreStyles.ObsConfigurationObsTime,
              <.label("Observation time", HelpIcon("configuration/obstime.md")),
              Datepicker(onChange =
                (newValue, _) => newValue.fromDatePickerToInstantOpt.foldMap(obsInstant.set)
              )
                .showTimeInput(true)
                .selected(obsInstant.get.toDatePickerJsDate)
                .dateFormat("yyyy-MM-dd HH:mm"),
              "UTC"
            )
          ),
          Form(size = Small)(
            ExploreStyles.Compact,
            ExploreStyles.ExploreForm,
            ExploreStyles.ConfigurationForm
          )(
            <.label("Mode", HelpIcon("configuration/mode.md")),
            EnumViewSelect[ReuseView, ScienceMode](id = "configuration-mode", value = mode),
            SpectroscopyConfigurationPanel(spectroscopy.as(dataIso))
              .when(isSpectroscopy),
            ImagingConfigurationPanel(imaging)
              .unless(isSpectroscopy),
            SequenceEditorPopup(
              props.obsId,
              trigger = Reuse.by(props.obsId)(
                Button(
                  size = Small,
                  compact = true,
                  clazz = ExploreStyles.VeryCompact,
                  content = "View Sequence"
                )
              )
            )
          ),
          SpectroscopyModesTable(
            modeView,
            spectroscopy.get,
            props.constraints,
            if (props.itcTargets.isEmpty) none else props.itcTargets.some,
            ctx.staticData.spectroscopyMatrix
          ).when(isSpectroscopy)
        )
      }
}
