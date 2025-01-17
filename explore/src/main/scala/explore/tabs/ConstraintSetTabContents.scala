// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.ConstraintGroupQueries._
import explore.common.UserPreferencesQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.constraints.ConstraintsSummaryTable
import explore.implicits._
import explore.model._
import explore.model.enums.AppTab
import explore.model.reusability._
import explore.observationtree.ConstraintGroupObsList
import explore.optics._
import explore.optics.all._
import explore.syntax.ui._
import explore.undo._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils._
import org.scalajs.dom.window
import queries.common.ConstraintGroupQueriesGQL._
import queries.common.ObsQueriesGQL
import queries.common.UserPreferencesQueriesGQL._
import react.common.ReactFnProps
import react.draggable.Axis
import react.fa._
import react.resizable._
import react.resizeDetector.ResizeDetector
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet
import scala.concurrent.duration._

final case class ConstraintSetTabContents(
  userId:           Option[User.Id],
  programId:        Program.Id,
  focusedObsSet:    Option[ObsIdSet],
  expandedIds:      View[SortedSet[ObsIdSet]],
  listUndoStacks:   View[UndoStacks[IO, ConstraintGroupList]],
  // TODO: Clean up the groupUndoStack somewhere, somehow?
  groupUndoStack:   View[Map[ObsIdSet, UndoStacks[IO, ConstraintSet]]],
  hiddenColumns:    View[Set[String]],
  summarySorting:   View[List[(String, Boolean)]],
  size:             ResizeDetector.Dimensions
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConstraintSetTabContents](ConstraintSetTabContents.component)

object ConstraintSetTabContents {
  type Props = ConstraintSetTabContents
  type State = TwoPanelState

  def readWidthPreference(props: Props, state: View[State]): Callback = {
    implicit val ctx = props.ctx

    (UserAreaWidths.queryWithDefault[IO](
      props.userId,
      ResizableSection.ConstraintSetsTree,
      Constants.InitialTreeWidth.toInt
    ) >>= (w => state.zoom(TwoPanelState.treeWidth).async.set(w.toDouble))).runAsync
  }

  protected def renderFn(props: Props, state: View[State])(
    constraintsWithObs:         View[ConstraintSummaryWithObervations]
  )(implicit
    ctx:                        AppContextIO
  ): VdomNode = {
    val treeResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        (state.zoom(TwoPanelState.treeWidth).set(d.size.width.toDouble).to[IO] *>
          UserWidthsCreation
            .storeWidthPreference[IO](props.userId,
                                      ResizableSection.ConstraintSetsTree,
                                      d.size.width
            )).runAsync
          .debounce(1.second)

    val treeWidth = state.get.treeWidth.toInt

    def tree(constraintWithObs: View[ConstraintSummaryWithObervations]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel)(
        treeInner(constraintWithObs)
      )

    def treeInner(constraintWithObs: View[ConstraintSummaryWithObervations]) =
      <.div(ExploreStyles.TreeBody)(
        ConstraintGroupObsList(
          constraintWithObs,
          props.programId,
          props.focusedObsSet,
          state.zoom(TwoPanelState.selected).set(SelectedPanel.summary).reuseAlways,
          props.expandedIds,
          props.listUndoStacks
        )
      )

    def findConstraintGroup(
      obsIds: ObsIdSet,
      cgl:    ConstraintGroupList
    ): Option[ConstraintGroup] =
      cgl.find(_._1.intersect(obsIds).nonEmpty).map(_._2)

    def onModSummaryWithObs(
      groupObsIds:  ObsIdSet,
      editedObsIds: ObsIdSet
    )(cswo:         ConstraintSummaryWithObervations): Callback = {
      val groupList = cswo.constraintGroups

      val updateExpanded = findConstraintGroup(editedObsIds, groupList).fold(Callback.empty) { cg =>
        // We should always find the constraint group.
        // If a group was edited while closed and it didn't create a merger, keep it closed,
        // otherwise expand all affected groups.
        props.expandedIds
          .mod { eids =>
            val withOld       =
              if (groupObsIds === editedObsIds) eids
              else eids + groupObsIds.removeUnsafe(editedObsIds)
            val withOldAndNew =
              if (editedObsIds === cg.obsIds && editedObsIds === groupObsIds) withOld
              else withOld + cg.obsIds

            withOldAndNew.filter(ids => groupList.contains(ids)) // clean up
          }
      }

      updateExpanded
    }

    val backButton: VdomNode =
      Button(
        as = <.a,
        size = Mini,
        compact = true,
        basic = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE = linkOverride[ButtonProps](
          ctx.pushPage(AppTab.Constraints, props.programId, Focused.None) >>
            state.zoom(TwoPanelState.selected).set(SelectedPanel.tree)
        )
      )(^.href := ctx.pageUrl(AppTab.Constraints, props.programId, Focused.None), Icons.ChevronLeft)

    val coreWidth  = props.size.width.getOrElse(0) - treeWidth
    val coreHeight = props.size.height.getOrElse(0)

    val rightSide = props.focusedObsSet
      .flatMap(ids =>
        findConstraintGroup(ids, constraintsWithObs.get.constraintGroups).map(cg => (ids, cg))
      )
      .fold[VdomNode] {
        Tile("constraints".refined,
             "Constraints Summary",
             backButton.some,
             key = "constraintsSummary"
        )(renderInTitle =>
          ConstraintsSummaryTable(
            props.programId,
            constraintsWithObs.get.constraintGroups,
            props.hiddenColumns,
            props.summarySorting,
            props.expandedIds,
            renderInTitle
          )
        )
      } { case (idsToEdit, constraintGroup) =>
        val groupObsIds   = constraintGroup.obsIds
        val constraintSet = constraintGroup.constraintSet
        val cglView       = constraintsWithObs
          .withOnMod(onModSummaryWithObs(groupObsIds, idsToEdit))
          .zoom(ConstraintSummaryWithObervations.constraintGroups)

        val getCs: ConstraintGroupList => ConstraintSet = _ => constraintSet

        def modCs(mod: ConstraintSet => ConstraintSet): ConstraintGroupList => ConstraintGroupList =
          cgl =>
            findConstraintGroup(idsToEdit, cgl)
              .map { cg =>
                val newCg        = ConstraintGroup.constraintSet.modify(mod)(cg)
                // see if the edit caused a merger
                val mergeWithIds = cgl
                  .find { case (ids, group) =>
                    !ids.intersects(idsToEdit) && group.constraintSet === newCg.constraintSet
                  }
                  .map(_._1)

                // If we're editing an observation within a larger group, we need a split
                val splitList =
                  if (idsToEdit === groupObsIds)
                    cgl.updated(groupObsIds, newCg) // otherwise, just update current group
                  else {
                    val diffIds = groupObsIds.removeUnsafe(idsToEdit)
                    cgl
                      .removed(groupObsIds)
                      .updated(idsToEdit, ConstraintGroup(newCg.constraintSet, idsToEdit))
                      .updated(diffIds, ConstraintGroup(cg.constraintSet, diffIds))
                  }

                mergeWithIds.fold(splitList) { idsToMerge =>
                  val combined = idsToMerge ++ idsToEdit
                  splitList
                    .removed(idsToMerge)
                    .removed(idsToEdit)
                    .updated(combined, ConstraintGroup(newCg.constraintSet, combined))
                }
              }
              .getOrElse(cgl) // shouldn't happen

        val csView: View[ConstraintSet] =
          cglView
            .zoom(getCs)(modCs)

        val csUndo: View[UndoStacks[IO, ConstraintSet]] =
          props.groupUndoStack.zoom(atMapWithDefault(idsToEdit, UndoStacks.empty))

        val title = idsToEdit.single match {
          case Some(id) => s"Observation $id"
          case None     => s"Editing Constraints for ${idsToEdit.size} Observations"
        }

        Tile("constraints".refined, title, backButton.some)(renderInTitle =>
          ConstraintsPanel(idsToEdit.toList, csView, csUndo, renderInTitle)
        )
      }

    if (window.canFitTwoPanels) {
      <.div(^.key := "constraints-base")(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, tree(constraintsWithObs))
          .when(state.get.selected.leftPanelVisible),
        <.div(^.key := "constraintset-right-side", ExploreStyles.SinglePanelTile)(
          rightSide
        ).when(state.get.selected.rightPanelVisible)
      )
    } else {
      <.div(
        ExploreStyles.TreeRGL,
        Resizable(
          axis = Axis.X,
          width = treeWidth.toDouble,
          height = coreHeight.toDouble,
          minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
          maxConstraints = (props.size.width.getOrElse(0) / 2, 0),
          onResize = treeResize,
          resizeHandles = List(ResizeHandleAxis.East),
          content = tree(constraintsWithObs),
          clazz = ExploreStyles.ResizableSeparator
        ),
        <.div(^.key   := "constraintset-right-side",
              ExploreStyles.SinglePanelTile,
              ^.width := coreWidth.px,
              ^.left  := treeWidth.px
        )(
          rightSide
        )
      )
    }
  }

  protected implicit val innerWidthReuse: Reusability[Double] = Reusability.double(2.0)

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(TwoPanelState.initial(SelectedPanel.Uninitialized))
      .useEffectOnMountBy((props, state) => readWidthPreference(props, state))
      .useEffectWithDepsBy((props, state) =>
        (props.focusedObsSet, state.zoom(TwoPanelState.selected).reuseByValue)
      ) { (_, _) => params =>
        val (focusedObsSet, selected) = params
        (focusedObsSet, selected.get) match {
          case (Some(_), _)                 => selected.set(SelectedPanel.editor)
          case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.summary)
          case _                            => Callback.empty
        }
      }
      .useStreamResourceViewOnMountBy { (props, _) =>
        implicit val ctx = props.ctx

        ConstraintGroupObsQuery
          .query(props.programId)
          .map(ConstraintGroupObsQuery.Data.asConstraintSummWithObs.get)
          .reRunOnResourceSignals(
            ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO](props.programId)
          )
      }
      .render { (props, state, constraintsWithObs) =>
        implicit val ctx = props.ctx

        constraintsWithObs.render(renderFn(props, state) _)
      }
}
