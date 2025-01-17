// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import coulomb.policy.spire.standard.given
import explore.model.formats._
import lucuma.core.arb._
import lucuma.core.math.Parallax
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline.FormatTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen

class ModelFormatsSuite extends DisciplineSuite {
  import ArbParallax._

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],             // swap for a random string
      s => Gen.const(s.replace("2", "0")) // create a leading zero, maybe (ok)
    )

  val parallaxMilliArcSecondsGen: Gen[String] =
    arbitrary[Parallax]
      .map(_.mas.toValue[BigDecimal].value.toString)
      .flatMapOneOf(Gen.const[String], perturbations: _*)

  checkAll("pxFormat", FormatTests(pxFormat).formatWith(parallaxMilliArcSecondsGen))
}
