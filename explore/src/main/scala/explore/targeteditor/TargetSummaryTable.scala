// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.implicits._
import explore.common.TargetObsQueries
import explore.common.TargetObsQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
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
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import reactST.reactTable._
import react.common._
import explore.model.reusability._

import scalajs.js
import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  pointingsWithObs: PointingsWithObs,
  focused:          View[Option[Focused]],
  expandedIds:      View[ExpandedIds],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactProps[TargetSummaryTable](TargetSummaryTable.component)

object TargetSummaryTable {
  type Props = TargetSummaryTable

  private val TargetTable = TableMaker[TargetResult].withSort

  private val TargetTableComponent = new SUITable(TargetTable)

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  // TODO Generalize and move this logic to react-common (ReactProps receiving a FnComponent as parameter)
  // implicit def render(props: TargetSummaryTable): VdomElement = component(props).vdomElement

  protected class Backend {

    def render(props: Props) = {
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

      // def columnWithObs[V](id: String, accessor: (TargetResult, List[ObsResult]) => V) =
      //   TargetTable.Column(id, { case (t, obs) => accessor(t, obs) }).setHeader(columnNames(id))

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
              column(
                "count",
                target => targetObservations(target.id).length
              ),
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

      // All this is a hack while we don't have proper hooks.
      val rawData = props.pointingsWithObs.targets.toList.toJSArray

      val reuseBy = (
        props.pointingsWithObs.targets.toList.map(_.toString) ++
          props.pointingsWithObs.observations.toList.map(_.toString)
      ).toJSArray

      println(rawData)
      println(reuseBy)

      val data = React.raw
        .asInstanceOf[js.Dynamic]
        .useMemo(() => rawData, reuseBy)
        .asInstanceOf[js.Array[TargetResult]]

      val tableInstance = TargetTable.use(columns, data)

      <.div(
        props.renderInTitle(
          <.span(ExploreStyles.TitleStrip)(
            Dropdown(item = true,
                     simple = true,
                     pointing = Pointing.TopRight,
                     text = "Columns",
                     clazz = ExploreStyles.SelectColumns
            )(
              DropdownMenu()(
                tableInstance.allColumns
                  .drop(2)
                  .toTagMod(column =>
                    DropdownItem()(^.key := column.id.toString)(
                      <.div(
                        Checkbox(label = columnNames(column.id.toString),
                                 checked = column.isVisible,
                                 onChange = (_: Boolean) => Callback(column.toggleHidden())
                        )
                      )
                    )
                  )
              )
            )
          )
        ),
        tableComponent(
          (TargetTableComponent(
             Table(celled = true, selectable = true, striped = true, compact = TableCompact.Very),
             header = true
           ),
           tableInstance
          )
        )
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

  // Horrible hack while we don't fully have hooks.
  // Reusability is handled in class component, instead of the need to useMemo.
  // Table is only rerendered when needed, thus avoiding the loop in react-table when passing unstable columns or data.
  private val tableComponent =
    ScalaFnComponent[(TargetTableComponent.Applied, TargetTable.InstanceType)] { props =>
      props._1(props._2)
    }
}
