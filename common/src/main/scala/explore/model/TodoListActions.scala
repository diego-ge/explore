// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import crystal._
import cats.effect.Async
import cats.implicits._
import explore.graphql._
import clue.GraphQLClient
import diode.data._

trait TodoListActions[F[_]] {
  def retrieveAll(): F[List[Task]]
  def refresh(): F[Unit]
  def toggle(id: String): F[Unit]
}

class TodoListActionsInterpreter[F[_]: Async](lens: FixedLens[F, Pot[List[Task]]])(
  todoClient:                                       GraphQLClient[F]
) extends TodoListActions[F] {
  def retrieveAll(): F[List[Task]] = {
    val result = todoClient.query(AllTasksQuery)()
    result.map(_.todos)
  }

  def refresh(): F[Unit] =
    for {
      _     <- lens.set(Pending())
      tasks <- retrieveAll()
      _     <- lens.set(Ready(tasks))
    } yield ()

  def toggle(id: String): F[Unit] =
    todoClient
      .query(ToggleMutation)(ToggleMutation.Variables(id).some)
      .map(_ => ())

}
