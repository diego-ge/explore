// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import explore._
import explore.components.ui.ExploreStyles
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.enums.AppTab
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.beautifuldnd._

trait ViewCommon {
  def programId: Program.Id
  def focusedObsSet: Option[ObsIdSet]

  def renderObsBadge(
    obs:               ObsSummary,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false // if true, overrides highlightSelected
  ): TagMod =
    ObsBadge(
      obs,
      selected = forceHighlight || (highlightSelected && focusedObsSet.exists(_.contains(obs.id)))
    )

  def renderObsBadgeItem(
    selectable:        Boolean,
    onSelect:          Observation.Id => Callback,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false,
    linkToObsTab:      Boolean = false,
    onCtrlClick:       Observation.Id => Callback = _ => Callback.empty
  )(
    obs:               ObsSummary,
    idx:               Int
  )(implicit ctx:      AppContextIO): TagMod =
    <.div(ExploreStyles.ObsTreeItem)(
      Draggable(obs.id.toString, idx) { case (provided, snapshot, _) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          getDraggedStyle(provided.draggableStyle, snapshot),
          (^.onClick ==> { (e: ReactMouseEvent) =>
            e.stopPropagationCB >>
              (if (e.ctrlKey || e.metaKey)
                 onCtrlClick(obs.id)
               else onSelect(obs.id))
          }).when(selectable),
          (^.onDoubleClick ==> { (e: ReactEvent) =>
            e.stopPropagationCB >>
              ctx.pushPage(AppTab.Observations, programId, Focused.singleObs(obs.id))
          }).when(linkToObsTab)
        )(<.span(provided.dragHandleProps)(renderObsBadge(obs, highlightSelected, forceHighlight)))
      }
    )

  // Adapted from https://github.com/atlassian/react-beautiful-dnd/issues/374#issuecomment-569817782
  def getDraggedStyle(style: TagMod, snapshot: Draggable.StateSnapshot): TagMod =
    if (!snapshot.isDragging)
      TagMod.empty
    else if (!snapshot.isDropAnimating)
      style
    else
      TagMod(style, ^.transitionDuration := "0.001s")

  def getListStyle(isDragging: Boolean): TagMod =
    ExploreStyles.DraggingOver.when(isDragging)
}
