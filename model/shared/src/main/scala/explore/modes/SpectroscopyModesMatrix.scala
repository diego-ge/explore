// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.data.NonEmptyList
import cats.implicits._
import coulomb._
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.numeric._
import eu.timepit.refined.types.string._
import explore.model.enum.SpectroscopyCapabilities
import fs2.data.csv._
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import monocle.Lens
import monocle.macros.GenLens
import spire.math.Rational

sealed trait FocalPlane extends Product with Serializable

object FocalPlane {
  case object SingleSlit   extends FocalPlane
  case object MultipleSlit extends FocalPlane
  case object IFU          extends FocalPlane

  /** @group Typeclass Instances */
  implicit val FocalPlaneEnumerated: Enumerated[FocalPlane] =
    Enumerated.of(SingleSlit, MultipleSlit, IFU)
}

case class SpectroscopyModeRow(
  instrument:        Instrument,
  config:            NonEmptyString,
  filterTag:         Option[NonEmptyString], // TODO Find a better type to represent any filter
  focalPlane:        NonEmptyList[FocalPlane],
  capabilities:      Option[SpectroscopyCapabilities],
  ao:                ModeAO,
  minWavelength:     ModeWavelength,
  maxWavelength:     ModeWavelength,
  optimalWavelength: ModeWavelength,
  wavelengthRange:   Quantity[NonNegBigDecimal, Micrometer],
  resolution:        PosBigDecimal,
  slitLength:        ModeSlitSize,
  slitWidth:         ModeSlitSize
)

object SpectroscopyModeRow {
  val instrument: Lens[SpectroscopyModeRow, Instrument] = GenLens[SpectroscopyModeRow](_.instrument)
}

trait SpectroscopyModesMatrixDecoders extends Decoders {
  implicit val nonEmptyStringDecoder: CellDecoder[NonEmptyString] =
    CellDecoder.stringDecoder
      .emap { x =>
        refineV[NonEmpty](x).leftMap(s => new DecoderError(s))
      }

  implicit val focalPlaneDecoder: CellDecoder[NonEmptyList[FocalPlane]] =
    CellDecoder.stringDecoder
      .emap { r =>
        r.toLowerCase
          .replace("\\s", "")
          .trim
          .split(",")
          .toList
          .collect {
            case "singleslit" => FocalPlane.SingleSlit
            case "multislit"  => FocalPlane.MultipleSlit
            case "ifu"        => FocalPlane.IFU
          } match {
          case h :: t => NonEmptyList.of(h, t: _*).asRight
          case Nil    => new DecoderError(s"Unknown focal plane $r").asLeft
        }
      }

  implicit val capabilitiesDecoder: CellDecoder[Option[SpectroscopyCapabilities]] =
    CellDecoder.stringDecoder
      .map {
        case "Nod&Shuffle" => SpectroscopyCapabilities.NodAndShuffle.some
        case "coronagraph" => SpectroscopyCapabilities.Corongraphy.some
        case _             => none
      }

  implicit object SpectroscopySpectroscopyModeRowDecoder
      extends CsvRowDecoder[SpectroscopyModeRow, String] {
    def apply(row: CsvRow[String]): DecoderResult[SpectroscopyModeRow] =
      for {
        i   <- row.as[Instrument]("instrument")
        s   <- row.as[NonEmptyString]("Config")
        fi  <- row.as[NonEmptyString]("filter").map {
                 case n if n.value.toLowerCase === "none" => none
                 case f                                   => f.some
               }
        f   <- row.as[NonEmptyList[FocalPlane]]("Focal Plane")
        c   <- row.as[Option[SpectroscopyCapabilities]]("capabilities")
        a   <- row.as[ModeAO]("AO")
        min <- row.as[ModeWavelength]("wave min")
        max <- row.as[ModeWavelength]("wave max")
        wo  <- row.as[ModeWavelength]("wave optimal")
        wr  <- row.as[NonNegBigDecimal]("wave range").map(_.withUnit[Micrometer])
        r   <- row.as[PosBigDecimal]("resolution")
        sl  <- row.as[ModeSlitSize]("slit length")
        sw  <- row.as[ModeSlitSize]("slit width")
      } yield SpectroscopyModeRow(i, s, fi, f, c, a, min, max, wo, wr, r, sl, sw)
  }
}

final case class SpectroscopyModesMatrix(matrix: List[SpectroscopyModeRow]) {
  val ScoreBump   = Rational(1, 2)
  val FilterLimit = Wavelength.fromNanometers(650)

  def filtered(
    focalPlane:   Option[FocalPlane],
    capabilities: Option[SpectroscopyCapabilities],
    iq:           Option[ImageQuality],
    wavelength:   Option[Wavelength],
    resolution:   Option[PosBigDecimal],
    range:        Option[Quantity[NonNegBigDecimal, Micrometer]],
    slitWidth:    Option[Angle]
  ): List[SpectroscopyModeRow] = {
    // Criteria to filter the modes
    val filter: SpectroscopyModeRow => Boolean = r => {
      focalPlane.forall(f => r.focalPlane.exists(_ === f)) &&
        r.capabilities === capabilities &&
        iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.PointTwo)) &&
        wavelength.forall(w => w >= r.minWavelength.w && w <= r.maxWavelength.w) &&
        resolution.forall(_ <= r.resolution) &&
        range.forall(_ <= r.wavelengthRange) &&
        slitWidth.forall(_.toMicroarcseconds <= r.slitLength.sw.toMicroarcseconds)
    }

    // Calculates a score for each mode for sorting purposes. It is down in Rational space, we may change it to double as we don't really need high precission for this
    val score: SpectroscopyModeRow => Rational = { r =>
      // Difference in wavelength
      val deltaWave: Rational       =
        wavelength
          .map(w => (r.optimalWavelength.w.nanometer - w.nanometer).value.abs)
          .getOrElse(Rational.zero)
      // Difference in slit width
      val deltaSlitWidth: Rational  =
        iq.map(i =>
          (Rational(r.slitWidth.sw.toMicroarcseconds, 1000000) - i.toArcSeconds.value).abs
        ).getOrElse(Rational.zero)
      // Difference in resolution
      val deltaRes: BigDecimal      =
        resolution.foldMap(re => (re.value - r.resolution.value).abs)
      // give a bumpp to non-AO modes (but don't discard them)
      val aoScore: Rational         =
        if (iq.forall(i => r.ao =!= ModeAO.AO || (i <= ImageQuality.PointTwo))) ScoreBump
        else Rational.zero
      // If wavelength > 0.65mu, then prefer settings with a filter to avoid 2nd order contamination
      val filterScore: Rational     =
        (wavelength, FilterLimit)
          .mapN { (w, l) =>
            if (w >= l && r.filterTag.isDefined) ScoreBump else Rational.zero
          }
          .getOrElse(Rational.zero)
      // Wavelength matche
      val wavelengthScore: Rational = wavelength
        .map(w => (w.nanometer.value / (w.nanometer.value + deltaWave)))
        .getOrElse(Rational.zero)
      // Resolution match
      val resolutionScore: Rational = resolution
        .map(r => Rational(r.value / (r.value + deltaRes)))
        .getOrElse(Rational.zero)
      // Slit width match to the seeing (the IFU always matches)
      val slitWidthScore            =
        if (r.focalPlane.forall(_ === FocalPlane.IFU)) Rational.one
        else
          iq.map(i => Rational(i.toArcSeconds.value / (i.toArcSeconds.value + deltaSlitWidth)))
            .getOrElse(Rational.zero)
      aoScore + wavelengthScore + filterScore + resolutionScore + slitWidthScore
    }

    matrix
      .filter(filter)  // Only include matches
      .fproduct(score) // Give it a score
      .sortBy(_._2)    // Sort by score
      .map(_._1)
      .reverse
  }
}

object SpectroscopyModesMatrix extends SpectroscopyModesMatrixPlatform {
  val empty: SpectroscopyModesMatrix = SpectroscopyModesMatrix(Nil)
}
