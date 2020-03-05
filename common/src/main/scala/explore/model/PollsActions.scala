// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import crystal._
import cats.effect.ConcurrentEffect
import cats.implicits._
import explore.graphql.polls._
import java.util.UUID
import clue._
import diode.data._

trait PollsActions[F[_]] {
  def retrieveAll(): F[List[Poll]]
  def refresh(): F[Unit]
  def vote(optionId: UUID): F[Unit]
}

class PollsActionsInterpreter[F[_]: ConcurrentEffect](lens: FixedLens[F, Pot[List[Poll]]])(
  pollsClient:                                              GraphQLClient[F]
) extends PollsActions[F] {

  def retrieveAll(): F[List[Poll]] =
    pollsClient.query(PollsQuery)().map(_.poll)

  def refresh(): F[Unit] =
    for {
      _     <- lens.set(Pending())
      polls <- retrieveAll()
      _     <- lens.set(Ready(polls))
    } yield ()

  def vote(optionId: UUID): F[Unit] =
    pollsClient
      .query(VoteMutation)(
        VoteMutation
          .Variables(optionId, UUID.fromString("664ccbe7-b3da-9865-e7cf-8e64ea91897d"))
          .some
      )
      .map(_ => ())
}
