package explore.model

import cats.Eq
import explore.model.decoders._
import io.circe.Decoder
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import monocle.Lens
import monocle.Focus

case class TargetEnv(id: TargetEnvironment.Id, scienceTargets: List[ScienceTarget])

object TargetEnv {
  implicit val eqTargetEnv: Eq[TargetEnv] = Eq.by(x => (x.id, x.scienceTargets))

  implicit val decoderTargetEnv: Decoder[TargetEnv] = Decoder.instance(c =>
    for {
      id             <- c.downField("id").as[TargetEnvironment.Id]
      scienceTargets <- c.downField("scienceTargets").as[List[ScienceTarget]]
    } yield TargetEnv(id, scienceTargets)
  )

  val id: Lens[TargetEnv, TargetEnvironment.Id]            = Focus[TargetEnv](_.id)
  val scienceTargets: Lens[TargetEnv, List[ScienceTarget]] = Focus[TargetEnv](_.scienceTargets)
}

case class ScienceTarget(id: Target.Id, target: Target)
object ScienceTarget {
  implicit val eqScienceTarget: Eq[ScienceTarget] = Eq.by(x => (x.id, x.target))

  implicit val decoderTargetEnv: Decoder[ScienceTarget] = Decoder.instance(c =>
    for {
      id     <- c.downField("id").as[Target.Id]
      target <- c.as[Target]
    } yield ScienceTarget(id, target)
  )

  val id: Lens[ScienceTarget, Target.Id]  = Focus[ScienceTarget](_.id)
  val target: Lens[ScienceTarget, Target] = Focus[ScienceTarget](_.target)
}
