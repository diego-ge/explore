// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Order._
import cats.syntax.all._
import crystal.react.View
import crystal.react.reuse._
import explore.Icons
import explore.common.ConstraintGroupQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ConstraintGroup
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.enums.AppTab
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Program
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.collections.table._
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import reactST.reactTable._
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.IdType
import reactST.reactTable.mod.SortingRule

import scala.collection.immutable.SortedSet

import scalajs.js.JSConverters._

final case class ConstraintsSummaryTable(
  programId:        Program.Id,
  constraintList:   ConstraintGroupList,
  hiddenColumns:    View[Set[String]],
  summarySorting:   View[List[(String, Boolean)]],
  expandedIds:      View[SortedSet[ObsIdSet]],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConstraintsSummaryTable](ConstraintsSummaryTable.component)

object ConstraintsSummaryTable {
  type Props = ConstraintsSummaryTable

  protected val ConstraintsTable = TableDef[ConstraintGroup].withSortBy

  protected val ConstraintsTableComponent = new SUITable(ConstraintsTable)

  private val columnNames: Map[String, String] = Map(
    "edit"         -> " ",
    "iq"           -> "IQ",
    "cc"           -> "CC",
    "bg"           -> "BG",
    "wv"           -> "WV",
    "minam"        -> "Min AM",
    "maxam"        -> "Max AM",
    "minha"        -> "Min HA",
    "maxha"        -> "Max HA",
    "count"        -> "Count",
    "observations" -> "Observations"
  )

  private val columnClasses: Map[String, Css] = Map(
    "edit" -> (ExploreStyles.StickyColumn |+| ExploreStyles.ConstraintsSummaryEdit)
  )

  private def toSortingRules(tuples: List[(String, Boolean)]) = tuples.map { case (id, b) =>
    SortingRule[ConstraintGroup](id).setDesc(b)
  }

  private def fromTableState(state: ConstraintsTable.TableStateType): List[(String, Boolean)] =
    state.sortBy.toList
      .map(sr => (sr.id.toString, sr.desc.toOption.getOrElse(false)))

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemoBy(_ => ()) { props => _ => // Cols never changes, but needs access to props
        def column[V](id: String, accessor: ConstraintGroup => V) =
          ConstraintsTable
            .Column(id, accessor)
            .setHeader(columnNames(id))

        def setObsSet(obsIdSet: ObsIdSet): Callback =
          props.ctx.pushPage(AppTab.Constraints, props.programId, Focused.obsSet(obsIdSet))

        List(
          column("edit", ConstraintGroup.obsIds.get)
            .setCell(cell => <.a(^.onClick ==> (_ => setObsSet(cell.value)), Icons.Edit))
            .setDisableSortBy(true),
          column("iq", ConstraintGroup.constraintSet.andThen(ConstraintSet.imageQuality).get)
            .setCell(_.value.label)
            .setSortByFn(_.label),
          column("cc", ConstraintGroup.constraintSet.andThen(ConstraintSet.cloudExtinction).get)
            .setCell(_.value.label)
            .setSortByFn(_.label),
          column("bg", ConstraintGroup.constraintSet.andThen(ConstraintSet.skyBackground).get)
            .setCell(_.value.label)
            .setSortByFn(_.label),
          column("wv", ConstraintGroup.constraintSet.andThen(ConstraintSet.waterVapor).get)
            .setCell(_.value.label)
            .setSortByFn(_.label),
          column("minam", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
            .setCell(_.value match {
              case ElevationRange.AirMass(min, _) => f"${min.value}%.1f"
              case ElevationRange.HourAngle(_, _) => ""
            })
            .setSortByFn(_ match {
              case ElevationRange.AirMass(min, _) => min.value
              case ElevationRange.HourAngle(_, _) => ElevationRange.AirMass.MinValue - 1
            }),
          column("maxam", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
            .setCell(_.value match {
              case ElevationRange.AirMass(_, max) => f"${max.value}%.1f"
              case ElevationRange.HourAngle(_, _) => ""
            })
            .setSortByFn(_ match {
              case ElevationRange.AirMass(_, max) => max.value
              case ElevationRange.HourAngle(_, _) => ElevationRange.AirMass.MinValue - 1
            }),
          column("minha", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
            .setCell(_.value match {
              case ElevationRange.AirMass(_, _)     => ""
              case ElevationRange.HourAngle(min, _) => f"${min.value}%.1f"
            })
            .setSortByFn(_ match {
              case ElevationRange.AirMass(_, _)     => ElevationRange.HourAngle.MinHour - 1
              case ElevationRange.HourAngle(min, _) => min.value
            }),
          column("maxha", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
            .setCell(_.value match {
              case ElevationRange.AirMass(_, _)     => ""
              case ElevationRange.HourAngle(_, max) => f"${max.value}%.1f"
            })
            .setSortByFn(_ match {
              case ElevationRange.AirMass(_, _)     => ElevationRange.HourAngle.MinHour - 1
              case ElevationRange.HourAngle(_, max) => max.value
            }),
          column("count", _.obsIds.length)
            .setSortType(DefaultSortTypes.number),
          column("observations", ConstraintGroup.obsIds.get)
            .setCell(cell =>
              <.span(
                cell.value.toSortedSet.toList
                  .map(obsId =>
                    <.a(
                      ^.onClick ==> (_ =>
                        (props.ctx.pushPage(
                          AppTab.Constraints,
                          props.programId,
                          Focused.singleObs(obsId)
                        )
                          >> props.expandedIds.mod(_ + cell.value)
                          >> setObsSet(ObsIdSet.one(obsId)))
                      ),
                      obsId.toString
                    )
                  )
                  .mkReactFragment(", ")
              )
            )
            .setDisableSortBy(true)
        )
      }
      .useMemoBy((props, _) => props.constraintList)((_, _) => _.values.toList) // Memo rows
      .useTableBy((props, cols, rows) =>
        ConstraintsTable(
          cols,
          rows,
          { (hiddenColumns: Set[String], options: ConstraintsTable.OptionsType) =>
            options
              .setAutoResetSortBy(false)
              .setInitialState(
                ConstraintsTable
                  .State()
                  .setHiddenColumns(
                    hiddenColumns.toList.map(col => col: IdType[ConstraintGroup]).toJSArray
                  )
                  .setSortBy(toSortingRules(props.summarySorting.get): _*)
              )
          }.reuseCurrying(props.hiddenColumns.get)
        )
      )
      .useEffectWithDepsBy((_, _, _, tableInstance) => fromTableState(tableInstance.state))(
        (props, _, _, _) => rules => props.summarySorting.set(rules)
      )
      .render((props, _, _, tableInstance) =>
        <.div(
          props.renderInTitle(
            React.Fragment(
              <.span, // Push column selector to right
              <.span(ExploreStyles.TitleSelectColumns)(
                Dropdown(
                  item = true,
                  simple = true,
                  pointing = Pointing.TopRight,
                  scrolling = true,
                  text = "Columns",
                  clazz = ExploreStyles.SelectColumns
                )(
                  DropdownMenu(
                    tableInstance.allColumns
                      .drop(1)
                      .toTagMod { column =>
                        val colId = column.id.toString
                        DropdownItem()(^.key := colId)(
                          <.div(
                            Checkbox(
                              label = columnNames(colId),
                              checked = column.isVisible,
                              onChange = (value: Boolean) =>
                                Callback(column.toggleHidden()) >>
                                  props.hiddenColumns
                                    .mod(cols => if (value) cols - colId else cols + colId)
                            )
                          )
                        )
                      }
                  )
                )
              )
            )
          ),
          ConstraintsTableComponent(
            table = Table(celled = true,
                          selectable = true,
                          striped = true,
                          compact = TableCompact.Very
            )(),
            header = true,
            headerCell = (col: ConstraintsTable.ColumnType) =>
              TableHeaderCell(clazz =
                columnClasses.get(col.id.toString).orEmpty |+| ExploreStyles.StickyHeader
              )(
                ^.textTransform.none,
                ^.whiteSpace.nowrap
              ),
            cell = (cell: ConstraintsTable.CellType[?]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orEmpty)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
