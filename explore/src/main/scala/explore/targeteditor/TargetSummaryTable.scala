// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.implicits._
import explore.common.TargetObsQueries
import explore.common.TargetObsQueries._
import explore.implicits._
import explore.model.ExpandedIds
import explore.model.Focused
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.MagnitudeValue
import lucuma.core.model.Magnitude
import lucuma.core.model.Target
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
import react.semanticui.collections.table._
import reactST.reactTable._

import scalajs.js
import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  pointingsWithObs: PointingsWithObs,
  focused:          View[Option[Focused]],
  expandedIds:      View[ExpandedIds]
)(implicit val ctx: AppContextIO)

object TargetSummaryTable {
  type Props = TargetSummaryTable

  private val TargetTable = TableMaker[TargetResult]

  private val TargetTableComponent = new SUITable(TargetTable)

  // implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  // TODO Generalize and move this logic to react-common (ReactProps receiving a FnComponent as parameter)
  implicit def render(props: TargetSummaryTable): VdomElement = component(props).vdomElement

  val component = ScalaFnComponent[Props] { props =>
    implicit val ctx = props.ctx

    def targetObservations(id: Target.Id): List[ObsResult] =
      props.pointingsWithObs.observations.toList.filter(_.pointing match {
        case Some(PointingTargetResult(tid)) => tid === id
        case _                               => false
      })

    val columnNames = Map("icon" -> " ",
                          "name"         -> "Name",
                          "ra"           -> "RA",
                          "dec"          -> "Dec",
                          "priority"     -> "Priority",
                          "vmag"         -> "Vmag",
                          "count"        -> "Count",
                          "observations" -> "Observations"
    )

    def column[V](id: String, accessor: TargetResult => V) =
      TargetTable.Column(id, accessor).setHeader(columnNames(id))

    val columns = React.raw
      .asInstanceOf[js.Dynamic]
      .useMemo(
        () =>
          List(
            column("icon", _ => ""),
            column(
              "name",
              target =>
                <.a(^.onClick ==> (_ =>
                      props.focused.set(Focused.FocusedTarget(target.id).some).runAsyncCB
                    ),
                    target.name.value
                ).rawElement
            ),
            column(
              "ra",
              (TargetObsQueries.baseCoordinatesRa.get _)
                .andThen(TruncatedRA.rightAscension.get)
                .andThen(ValidFormatInput.truncatedRA.reverseGet)
            ),
            column(
              "dec",
              (TargetObsQueries.baseCoordinatesDec.get _)
                .andThen(TruncatedDec.declination.get)
                .andThen(ValidFormatInput.truncatedDec.reverseGet)
            ),
            column("priority", _ => ""),
            column(
              "vmag",
              _.magnitudes.collectFirst {
                case Magnitude(value, band, _, _) if band === MagnitudeBand.V =>
                  MagnitudeValue.fromString.reverseGet(value)
              }.orEmpty
            ),
            column("count", target => targetObservations(target.id).length),
            column(
              "observations",
              target =>
                <.span(
                  targetObservations(target.id)
                    .map(obs =>
                      <.a(
                        ^.onClick ==> (_ =>
                          (props.focused
                            .set(Focused.FocusedObs(obs.id).some) >> props.expandedIds
                            .mod(ExpandedIds.targetIds.modify(_ + target.id))).runAsyncCB
                        ),
                        obs.id.toString()
                      )
                    )
                    .mkReactFragment(", ")
                ).rawElement
            )
          ).toJSArray,
        js.Array()
      )
      .asInstanceOf[js.Array[TargetTable.ColumnOptionsType]]

    val data = React.raw
      .asInstanceOf[js.Dynamic]
      .useMemo(
        () => props.pointingsWithObs.targets.toList.toJSArray,
        js.Array()
      )
      .asInstanceOf[js.Array[TargetResult]]

    val tableInstance = TargetTable.use(columns, data)

    <.div(
      tableInstance.allColumns
        .drop(2)
        .toTagMod(column =>
          <.label(
            <.input(^.tpe := "checkbox",
                    util.props2Attrs(column.getToggleHiddenProps().asInstanceOf[js.Object])
            ),
            columnNames(column.id.toString)
          )
        ),
      TargetTableComponent(
        Table(celled = true, selectable = true, striped = true, compact = TableCompact.Very),
        header = true
      )(tableInstance)
    )
  }

}
