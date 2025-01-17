// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.effect._
import cats.syntax.all._
import clue._
import clue.js.FetchJSBackend
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.SSOClient
import explore.model.enums.AppTab
import explore.model.enums.ExecutionEnvironment
import explore.utils
import io.circe.Json
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.SetRouteVia
import lucuma.core.model.Program
import lucuma.schemas._
import org.http4s._
import org.typelevel.log4cats.Logger
import queries.schemas._
import workers.WebWorkerF

case class Clients[F[_]: Async: Parallel] protected (
  odb:           WebSocketClient[F, ObservationDB],
  preferencesDB: WebSocketClient[F, UserPreferencesDB],
  itc:           TransactionalClient[F, ITC]
) {
  def init(payload: Map[String, Json]): F[Unit] =
    (
      preferencesDB.connect() >> preferencesDB.initialize(),
      odb.connect() >> odb.initialize(payload)
    ).parTupled.void

  def close(): F[Unit] =
    List(
      preferencesDB.terminate() >> preferencesDB.disconnect(WebSocketCloseParams(code = 1000)),
      odb.terminate() >> odb.disconnect(WebSocketCloseParams(code = 1000))
    ).sequence.void
}
object Clients {
  def build[F[_]: Async: TransactionalBackend: WebSocketBackend: Parallel: Logger](
    odbURI:               Uri,
    prefsURI:             Uri,
    itcURI:               Uri,
    reconnectionStrategy: WebSocketReconnectionStrategy
  ): F[Clients[F]] =
    for {
      odbClient   <-
        ApolloWebSocketClient.of[F, ObservationDB](odbURI, "ODB", reconnectionStrategy)
      prefsClient <-
        ApolloWebSocketClient.of[F, UserPreferencesDB](prefsURI, "PREFS", reconnectionStrategy)
      itcClient   <-
        TransactionalClient.of[F, ITC](itcURI, "ITC")
    } yield Clients(odbClient, prefsClient, itcClient)
}

case class AppContext[F[_]](
  version:     NonEmptyString,
  clients:     Clients[F],
  sso:         SSOClient[F],
  pageUrl:     (AppTab, Program.Id, Focused) => String,
  setPageVia:  (AppTab, Program.Id, Focused, SetRouteVia) => Callback,
  environment: ExecutionEnvironment,
  worker:      WebWorkerF[F] // There will be a few workers in the future
)(implicit
  val F:       Applicative[F],
  val logger:  Logger[F],
  val P:       Parallel[F]
) {
  def pushPage(appTab: AppTab, programId: Program.Id, focused: Focused): Callback =
    setPageVia(appTab, programId, focused, SetRouteVia.HistoryPush)

  def replacePage(appTab: AppTab, programId: Program.Id, focused: Focused): Callback =
    setPageVia(appTab, programId, focused, SetRouteVia.HistoryReplace)
}

object AppContext {
  def from[F[_]: Async: FetchJSBackend: WebSocketBackend: Parallel: Logger](
    config:               AppConfig,
    reconnectionStrategy: WebSocketReconnectionStrategy,
    pageUrl:              (AppTab, Program.Id, Focused) => String,
    setPageVia:           (AppTab, Program.Id, Focused, SetRouteVia) => Callback,
    worker:               WebWorkerF[F]
  ): F[AppContext[F]] =
    for {
      clients <-
        Clients
          .build[F](config.odbURI, config.preferencesDBURI, config.itcURI, reconnectionStrategy)
      version  = utils.version(config.environment)
    } yield AppContext[F](
      version,
      clients,
      SSOClient(config.sso),
      pageUrl,
      setPageVia,
      config.environment,
      worker
    )
}
