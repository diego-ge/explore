// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.syntax.all._
import explore.model.enum.ObsStatus
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax._
import lucuma.core.model.Asterism
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.macros.Lenses

import java.time.Duration
import java.time.temporal.ChronoUnit

@Lenses
final case class ObsSummary(
  id:              Observation.Id,
  name:            Option[String],
  status:          ObsStatus = ObsStatus.New,
  conf:            String = "GMOS-N R831 1x300",
  constraints:     String = "<0.7\" <0.3 mag Bright",
  duration:        Duration = Duration.of(93, ChronoUnit.MINUTES),
  aimId:           Option[AimId],
  constraintSetId: Option[ConstraintSet.Id]
)

object ObsSummary {
  implicit val eqObsSummary: Eq[ObsSummary] = Eq.by(x => (x.id, x.name, x.aimId, x.constraintSetId))

  implicit val obsSummaryEncoder: Encoder[ObsSummary] = new Encoder[ObsSummary] {
    final def apply(c: ObsSummary): Json = {
      val common  = JsonObject(("id", c.id.asJson), ("name", c.name.asJson))
      val withAim = c.aimId match {
        case None    => common
        case Some(t) =>
          common.add(
            "observationTarget",
            t match {
              case Right(tid) =>
                Json.obj("type" -> AimType.Target.asJson, "target_id" -> tid.asJson)
              case Left(aid)  =>
                Json.obj("type" -> AimType.Asterism.asJson, "asterism_id" -> aid.asJson)
            }
          )
      }
      (c.constraintSetId match {
        case None       => withAim
        case Some(csId) => withAim.add("constraintSet", Json.obj("id" -> csId.asJson))
      }).asJson
    }

  }

  implicit val obsTargetDecoder: Decoder[AimId] =
    new Decoder[AimId] {
      final def apply(c: HCursor): Decoder.Result[AimId] =
        c.downField("type").as[AimType].flatMap {
          case AimType.Target   => c.downField("target_id").as[Target.Id].map(_.asRight)
          case AimType.Asterism => c.downField("asterism_id").as[Asterism.Id].map(_.asLeft)
        }
    }

  private case class CS(id: ConstraintSet.Id)

  private object CS {
    implicit val csDecoder: Decoder[CS] = new Decoder[CS] {
      final def apply(c: HCursor): Decoder.Result[CS] =
        for {
          id <- c.downField("id").as[ConstraintSet.Id]
        } yield (CS(id))
    }
  }

  implicit val obsSummaryDecoder: Decoder[ObsSummary] = new Decoder[ObsSummary] {
    final def apply(c: HCursor): Decoder.Result[ObsSummary] =
      for {
        id    <- c.downField("id").as[Observation.Id]
        name  <- c.downField("name").as[Option[String]]
        aimId <- c.downField("observationTarget").as[Option[AimId]]
        cs    <- c.downField("constraintSet").as[Option[CS]]
      } yield ObsSummary(id, name, aimId = aimId, constraintSetId = cs.map(_.id))
  }
}
