// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats._
import cats.effect.IO
import cats.instances.order._
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.ViewF
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.SimbadSearch
import explore.common.TargetObsQueries._
import explore.common.TargetObsQueriesGQL._
import explore.common.TargetQueries
import explore.common.TargetQueriesGQL
import explore.components.InputModal
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.ExpandedIds
import explore.model.Focused
import explore.model.Focused._
import explore.model.ObsSummaryWithConstraints
import explore.model.PointingId
import explore.model.reusability._
import explore.optics.GetAdjust
import explore.optics._
import explore.schemas.ObservationDB
import explore.schemas.ObservationDB.Types._
import explore.undo.KIListMod
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import monocle.function.Field1.first
import monocle.macros.Lenses
import monocle.std.option.some
import mouse.boolean._
import react.beautifuldnd._
import react.common._
import react.common.implicits._
import react.reflex._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.elements.header.Header
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.sizes._
import react.semanticui.views.card.Card
import react.semanticui.views.card.CardContent

import scala.collection.immutable.SortedSet
import scala.util.Random
import explore.components.undo.UndoButtons
import monocle.Getter

final case class TargetObsList(
  pointingsWithObs: View[PointingsWithObs],
  focused:          View[Option[Focused]],
  expandedIds:      View[ExpandedIds],
  searching:        View[Set[Target.Id]],
  undoStacks:       View[UndoStacks[IO, PointingsWithObs]]
)(implicit val ctx: AppContextIO)
    extends ReactProps[TargetObsList](TargetObsList.component)
    with ViewCommon

object TargetObsList {
  type Props = TargetObsList

  @Lenses
  case class State(
    dragging: Boolean = false
    // undoStacks: UndoStacks2[IO, PointingsWithObs] = UndoStacks2.empty
  )

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  val obsListMod            = new KIListMod[IO, ObsResult, Observation.Id](ObsResult.id)
  val targetListMod         = new KIListMod[IO, TargetResult, Target.Id](TargetResult.id)
  val asterismListMod       = new KIListMod[IO, AsterismIdName, Asterism.Id](AsterismIdName.id)
  val asterismTargetListMod =
    new KIListMod[IO, AsterismResultTarget, Target.Id](AsterismResultTarget.id)

  class Backend($ : BackendScope[Props, State]) {
    private val UnassignedObsId = "unassignedObs"

    def moveObs(obsId: Observation.Id, to: Option[PointingId])(implicit
      c:               TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      (to match {
        case Some(PointingId.TargetId(targetId))     =>
          AssignTargetToObs.execute(targetId, obsId)
        case Some(PointingId.AsterismId(asterismId)) =>
          AssignAsterismToObs.execute(asterismId, obsId)
        case None                                    => UnassignObs.execute(obsId)
      }).void

    def updateObs(input: EditObservationInput)(implicit
      c:                 TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      UpdateObservationMutation.execute(input).void

    def insertTarget(target: TargetResult)(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      AddTarget
        .execute(target.id, target.name)
        .handleErrorWith { _ =>
          UndeleteTarget.execute(target.id)
        }
        .void

    def deleteTarget(id: Target.Id)(implicit
      c:                 TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      DeleteTarget.execute(id).void

    def undeleteTarget(id: Target.Id)(implicit
      c:                   TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      UndeleteTarget.execute(id).void

    def insertAsterism(asterism: AsterismIdName)(implicit
      c:                         TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      AddAsterism
        .execute(asterism.id, asterism.name.orIgnore)
        .handleErrorWith { _ =>
          UndeleteAsterism.execute(asterism.id)
        }
        .void

    def deleteAsterism(id: Asterism.Id)(implicit
      c:                   TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      DeleteAsterism.execute(id).void

    def undeleteAsterism(id: Asterism.Id)(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      UndeleteAsterism.execute(id).void

    def shareTargetWithAsterism(targetId: Target.Id, asterismId: Asterism.Id)(implicit
      c:                                  TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      ShareTargetWithAsterisms.execute(targetId, asterismId).void

    private def unshareTargetWithAsterism(targetId: Target.Id, asterismId: Asterism.Id)(implicit
      c:                                            TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      UnshareTargetWithAsterisms.execute(targetId, asterismId).void

    protected def onDragEnd(
      setter:      UndoSetter[IO, PointingsWithObs],
      expandedIds: View[ExpandedIds]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => IO[Unit] =
      (result, _) =>
        $.propsIn[IO] >>= { props =>
          // println(scalajs.js.JSON.stringify(result))
          // We can drag:
          //  - An observation from a target or an asterism to another target or asterism.
          //  - A target into an asterism.

          result.destination.toOption
            .map(destination =>
              (Observation.Id.parse(result.draggableId) match {
                case Some(obsId) =>
                  // Observation dragged to a target or asterism.
                  val getAdjust: GetAdjust[PointingsWithObs, Option[Option[PointingId]]] =
                    PointingsWithObs.observations.composeGetAdjust(
                      obsListMod
                        .withKey(obsId)
                        .composeOptionLens(first) // Focus on Observation within ElemWithIndex
                        .composeOptionLens(targetsObsQueryObsPointingId)
                    )

                  val set: Option[Option[PointingId]] => IO[Unit] =
                    setter
                      .set[Option[Option[PointingId]]](
                        getAdjust.get,
                        getAdjust.set,
                        (_: Option[Option[PointingId]])
                          .map(newPointingId => moveObs(obsId, newPointingId))
                          .orEmpty
                      )

                  destination.droppableId match {
                    case UnassignedObsId            =>
                      set(none.some)
                    case Target.Id(newTargetId)     =>
                      expandedIds.zoom(ExpandedIds.targetIds).mod(_ + newTargetId) >>
                        set(PointingId.TargetId(newTargetId).some.some)
                    case Asterism.Id(newAsterismId) =>
                      expandedIds
                        .zoom(ExpandedIds.asterismIds)
                        .mod(_ + newAsterismId) >>
                        set(PointingId.AsterismId(newAsterismId).some.some)

                    case _ => IO.unit // Report error?
                  }
                case None        =>
                  Target.Id.parse(result.draggableId) match {
                    case Some(targetId) =>
                      // Target dragged to asterism.
                      Asterism.Id.parse(destination.droppableId) match {
                        case Some(asterismId) =>
                          props.pointingsWithObs.get.targets
                            .getElement(targetId)
                            .foldMap(target =>
                              expandedIds
                                .zoom(ExpandedIds.asterismIds)
                                .mod(_ + asterismId) >>
                                addTargetToAsterism(props.pointingsWithObs,
                                                    target,
                                                    asterismId,
                                                    setter
                                )
                            )
                        case None             => IO.unit
                      }
                    case None           => IO.unit
                  }
              })
            )
            .orEmpty
        }

    private def targetMod(
      setter:        UndoSetter[IO, PointingsWithObs],
      focused:       View[Option[Focused]],
      targetId:      Target.Id,
      focusOnDelete: Option[TargetResult]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): targetListMod.Operation => IO[Unit] = {
      val getAdjust: GetAdjust[PointingsWithObs, targetListMod.ElemWithIndex] =
        PointingsWithObs.targets.composeGetAdjust(
          targetListMod.withKey(targetId)
        )

      setter
        .mod[targetListMod.ElemWithIndex](
          getAdjust.get,
          getAdjust.set,
          onSet = _.fold(
            focused.set(focusOnDelete.map(f => Focused.FocusedTarget(f.id))) >>
              deleteTarget(targetId)
          ) { case (target, _) =>
            insertTarget(target) >> focused.set(FocusedTarget(targetId).some)
          },
          onRestore = (_: targetListMod.ElemWithIndex).fold(
            focused.set(focusOnDelete.map(f => Focused.FocusedTarget(f.id))) >>
              deleteTarget(targetId)
          ) { case (target, _) =>
            undeleteTarget(target.id) >> focused.set(FocusedTarget(target.id).some)
          }
        )
    }

    protected def newTarget(
      setter: UndoSetter[IO, PointingsWithObs]
    )(name:   NonEmptyString)(implicit
      c:      TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      ($.propsIn[IO], IO(PosLong.unsafeFrom(Random.nextInt(0xfff).abs.toLong + 1))).parTupled
        .flatMap { case (props, posLong) =>
          val newTarget =
            TargetResult(Target.Id(posLong),
                         name,
                         SiderealTracking.const(Coordinates.Zero),
                         List.empty
            )
          val mod       = targetMod(setter, props.focused, newTarget.id, none)
          (
            mod(targetListMod.upsert(newTarget, props.pointingsWithObs.get.targets.length)),
            props.searching.mod(_ + newTarget.id) >>
              SimbadSearch
                .search(name)
                .attempt
                .guarantee(props.searching.mod(_ - newTarget.id))
          ).parTupled.flatMap {
            case (_, Right(Some(Target(_, Right(st), m)))) =>
              val update = TargetQueries.UpdateSiderealTracking(st) >>>
                TargetQueries.updateMagnitudes(m.values.toList)
              TargetQueriesGQL.TargetMutation.execute(update(EditSiderealInput(newTarget.id))).void
            case _                                         =>
              IO.unit
          }
        }

    protected def deleteTarget(
      targetId:      Target.Id,
      setter:        UndoSetter[IO, PointingsWithObs],
      focusOnDelete: Option[TargetResult]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      $.propsIn[IO] >>= { props =>
        val mod = targetMod(setter, props.focused, targetId, focusOnDelete)
        mod(targetListMod.delete)
      }

    protected def newAsterism(
      setter: UndoSetter[IO, PointingsWithObs]
    )(name:   NonEmptyString)(implicit
      c:      TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      // Temporary measure until we have id pools.
      val newAsterism = IO(Random.nextInt()).map(int =>
        AsterismIdName(Asterism.Id(PosLong.unsafeFrom(int.abs.toLong + 1)),
                       name.some,
                       KeyedIndexedList.empty
        )
      )

      $.propsIn[IO] >>= { props =>
        newAsterism >>= { asterism =>
          val mod = asterismMod(setter, props.focused, asterism.id, none)
          mod(asterismListMod.upsert(asterism, props.pointingsWithObs.get.asterisms.length))
        }
      }
    }

    protected def deleteAsterism(
      asterismId:    Asterism.Id,
      setter:        UndoSetter[IO, PointingsWithObs],
      focusOnDelete: Option[AsterismIdName]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      $.propsIn[IO] >>= { props =>
        val mod =
          asterismMod(setter, props.focused, asterismId, focusOnDelete)
        mod(asterismListMod.delete)
      }

    private def asterismMod(
      setter:        UndoSetter[IO, PointingsWithObs],
      focused:       View[Option[Focused]],
      asterismId:    Asterism.Id,
      focusOnDelete: Option[AsterismIdName]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): asterismListMod.Operation => IO[Unit] = {
      val getAdjust: GetAdjust[PointingsWithObs, asterismListMod.ElemWithIndex] =
        PointingsWithObs.asterisms.composeGetAdjust(
          asterismListMod.withKey(asterismId)
        )

      setter
        .mod[asterismListMod.ElemWithIndex](
          getAdjust.get,
          getAdjust.set,
          onSet = _.fold(
            focused.set(focusOnDelete.map(f => FocusedAsterism(f.id))) >>
              deleteAsterism(asterismId)
          ) { case (asterism, _) =>
            insertAsterism(asterism) >> focused.set(FocusedAsterism(asterism.id).some)
          },
          onRestore = (_: asterismListMod.ElemWithIndex).fold(
            focused.set(focusOnDelete.map(f => FocusedAsterism(f.id))) >>
              deleteAsterism(asterismId)
          ) { case (asterism, _) =>
            undeleteAsterism(asterism.id) >> focused.set(FocusedAsterism(asterism.id).some)
          }
        )

    }

    private def asterismTargetMod(
      setter:     UndoSetter[IO, PointingsWithObs],
      targetId:   Target.Id,
      asterismId: Asterism.Id
    )(implicit
      c:          TransactionalClient[IO, ObservationDB]
    ): asterismTargetListMod.Operation => IO[Unit] = {
      val getAdjust: GetAdjust[PointingsWithObs, asterismListMod.ElemWithIndex] =
        PointingsWithObs.asterisms.composeGetAdjust(asterismListMod.withKey(asterismId))

      val targetWithId: GetAdjust[AsterismTargetList, asterismTargetListMod.ElemWithIndex] =
        asterismTargetListMod.withKey(targetId)

      val getter: Getter[PointingsWithObs, asterismTargetListMod.ElemWithIndex] =
        getAdjust.getter
          .map(_.map(_._1.targets).map(targetWithId.getter.get).flatten)

      val adjuster: Adjuster[PointingsWithObs, asterismTargetListMod.ElemWithIndex] =
        getAdjust.adjuster
          .composePrism(some)
          .composeLens(first)
          .composeLens(AsterismIdName.targets)
          .composeAdjuster(targetWithId.adjuster)

      setter
        .mod[asterismTargetListMod.ElemWithIndex](
          getter.get,
          adjuster.set,
          _.fold(
            unshareTargetWithAsterism(targetId, asterismId)
          ) { case (target, _) =>
            shareTargetWithAsterism(target.id, asterismId)
          }
        )
    }

    protected def addTargetToAsterism(
      pointingsWithObs: View[PointingsWithObs],
      target:           TargetResult,
      asterismId:       Asterism.Id,
      setter:           UndoSetter[IO, PointingsWithObs]
    )(implicit
      c:                TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      val mod = asterismTargetMod(setter, target.id, asterismId)
      mod(
        asterismTargetListMod.upsert(
          AsterismResultTarget(target.id, target.name),
          pointingsWithObs.get.asterisms.getElement(asterismId).foldMap(_.targets.length)
        )
      )
    }

    protected def deleteTargetFromAsterism(
      targetId:   Target.Id,
      asterismId: Asterism.Id,
      setter:     UndoSetter[IO, PointingsWithObs]
    )(implicit
      c:          TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      val mod = asterismTargetMod(setter, targetId, asterismId)
      mod(asterismTargetListMod.delete)
    }

    def toggleExpanded[A: Eq](
      id:          A,
      expandedIds: View[SortedSet[A]]
    ): IO[Unit] =
      expandedIds.mod { expanded =>
        expanded
          .exists(_ === id)
          .fold(expanded - id, expanded + id)
      }

    private def obsResultToObsSummary(obs: ObsResult): ObsSummaryWithConstraints =
      ObsSummaryWithConstraints(obs.id, obs.constraintSet)

    def render(props: Props) = {
      implicit val ctx = props.ctx

      val state   = ViewF.fromState[IO]($)
      val undoCtx = UndoContext(props.undoStacks, props.pointingsWithObs)

      val observations  = props.pointingsWithObs.get.observations
      val obsByPointing = observations.toList.groupBy(_.pointing)

      val targets        = props.pointingsWithObs.get.targets
      val targetsWithIdx = targets.toList.zipWithIndex

      val asterisms    = props.pointingsWithObs.get.asterisms.toList
      val asterismIds  = asterisms.map(_.id)
      val asterismIdxs = asterisms.zipWithIndex

      val unassignedObs = obsByPointing.get(none).orEmpty

      val renderClone: Draggable.Render =
        (provided, snapshot, rubric) => {
          <.div(provided.innerRef,
                provided.draggableProps,
                provided.dragHandleProps,
                props.getDraggedStyle(provided.draggableStyle, snapshot)
          )(
            (Target.Id
              .parse(rubric.draggableId)
              .toRight(Observation.Id.parse(rubric.draggableId)) match {
              case Right(targetId)   =>
                targets
                  .getElement(targetId)
                  .map(target => Card(raised = true)(CardContent(target.name.value)).vdomElement)
              case Left(Some(obsId)) =>
                observations
                  .getElement(obsId)
                  .map(obs => props.renderObsBadge(obsResultToObsSummary(obs)))
              case _                 => none
            }).getOrElse(<.span("ERROR"))
          )
        }

      val handleDragEnd = onDragEnd(undoCtx, props.expandedIds)

      def createTarget(name: NonEmptyString): Callback =
        newTarget(undoCtx)(name).runAsyncCB

      def createAsterism(name: NonEmptyString): Callback =
        newAsterism(undoCtx)(name).runAsyncCB

      DragDropContext(
        onDragStart =
          (_: DragStart, _: ResponderProvided) => state.zoom(State.dragging).set(true).runAsyncCB,
        onDragEnd = (result, provided) =>
          (state.zoom(State.dragging).set(false) >> handleDragEnd(result, provided)).runAsyncCB
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            <.div(
              InputModal(
                "Create new Target",
                initialValue = None,
                label = "Name",
                placeholder = "Target name",
                okLabel = "Create",
                onComplete = (createTarget _).reuseAlways, // TODO Set coordinates
                trigger = Button(size = Mini,
                                 compact = true,
                                 clazz = ExploreStyles.VeryCompact,
                                 icon = Icons.New,
                                 content = "Target"
                )
              ),
              InputModal(
                "Create new Asterism",
                initialValue = None,
                label = "Name",
                placeholder = "Asterism name",
                okLabel = "Create",
                onComplete = (createAsterism _).reuseAlways,
                trigger = Button(size = Mini,
                                 compact = true,
                                 clazz = ExploreStyles.VeryCompact,
                                 icon = Icons.New,
                                 content = "Asterism"
                )
              )
            ),
            UndoButtons(undoCtx, size = Mini)
          ),
          <.div(
            Button(onClick = props.focused.set(none).runAsyncCB,
                   clazz = ExploreStyles.ButtonSummary
            )(
              Icons.List,
              "Targets Summary"
            )
          ),
          ReflexContainer()(
            List[Option[VdomNode]](
              // Start Target Tree
              (ReflexElement(minSize = 36, clazz = ExploreStyles.ObsTreeSection)(
                Header(block = true, clazz = ExploreStyles.ObsTreeHeader)("Targets"),
                <.div(ExploreStyles.ObsTree)(
                  <.div(ExploreStyles.ObsScrollTree)(
                    targetsWithIdx.toTagMod { case (target, targetIdx) =>
                      val targetId      = target.id
                      val nextToSelect  = targetsWithIdx.find(_._2 === targetIdx + 1).map(_._1)
                      val prevToSelect  = targetsWithIdx.find(_._2 === targetIdx - 1).map(_._1)
                      val focusOnDelete = props.focused.get.collectFirst {
                        case FocusedTarget(tid) if tid === targetId =>
                          nextToSelect.orElse(prevToSelect)
                      }.flatten

                      val targetObs =
                        obsByPointing.get(PointingTargetResult(targetId).some).orEmpty

                      val expandedTargetIds =
                        props.expandedIds.zoom(ExpandedIds.targetIds)
                      val opIcon            =
                        targetObs.nonEmpty.fold(
                          Icon(
                            "chevron " + expandedTargetIds.get
                              .exists(_ === targetId)
                              .fold("down", "right")
                          )(^.cursor.pointer,
                            ^.onClick ==> { e: ReactEvent =>
                              e.stopPropagationCB >>
                                toggleExpanded(targetId, expandedTargetIds).runAsyncCB
                                  .asEventDefault(e)
                                  .void
                            }
                          ),
                          Icons.ChevronRight
                        )

                      val memberObsSelected = props.focused.get
                        .exists(f => targetObs.map(obs => FocusedObs(obs.id)).exists(f === _))

                      Droppable(targetId.toString, renderClone = renderClone) {
                        case (provided, snapshot) =>
                          // To implement "copy-drag", we use suggestion from
                          // https://github.com/atlassian/react-beautiful-dnd/issues/216#issuecomment-586266295
                          val shouldRenderClone =
                            snapshot.draggingFromThisWith.exists(
                              _ === targetId.toString
                            )

                          val targetHeader =
                            <.span(ExploreStyles.ObsTreeGroupHeader)(
                              <.span(ExploreStyles.ObsGroupTitle)(
                                opIcon,
                                target.name.value
                              ),
                              Button(
                                size = Small,
                                compact = true,
                                clazz = ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                                onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                                  e.stopPropagationCB >>
                                    deleteTarget(targetId, undoCtx, focusOnDelete).runAsyncCB
                              )(
                                Icons.Trash
                              ),
                              <.span(ExploreStyles.ObsCount, s"${targetObs.length} Obs")
                            )

                          <.div(
                            provided.innerRef,
                            provided.droppableProps,
                            props.getListStyle(
                              snapshot.draggingOverWith.exists(id =>
                                Observation.Id.parse(id).isDefined
                              )
                            )
                          )(
                            Segment(
                              vertical = true,
                              clazz = ExploreStyles.ObsTreeGroup
                                |+| Option
                                  .when(
                                    memberObsSelected || props.focused.get
                                      .exists(_ === FocusedTarget(targetId))
                                  )(ExploreStyles.SelectedObsTreeGroup)
                                  .orElse(
                                    Option.when(!state.get.dragging)(
                                      ExploreStyles.UnselectedObsTreeGroup
                                    )
                                  )
                                  .orEmpty
                            )(
                              ^.cursor.pointer,
                              ^.onClick --> props.focused
                                .set(FocusedTarget(targetId).some)
                                .runAsyncCB
                            )(
                              // Drag without removing from list.
                              if (asterisms.isEmpty || shouldRenderClone)
                                targetHeader
                              else
                                Draggable(targetId.toString, -1) {
                                  case (targetProvided, targetSnapshot, _) =>
                                    <.span(
                                      targetProvided.innerRef,
                                      targetProvided.draggableProps,
                                      props.getDraggedStyle(targetProvided.draggableStyle,
                                                            targetSnapshot
                                      ),
                                      targetProvided.dragHandleProps
                                    )(
                                      targetHeader
                                    )
                                },
                              TagMod
                                .when(expandedTargetIds.get.contains(targetId))(
                                  targetObs.zipWithIndex.toTagMod { case (obs, idx) =>
                                    props.renderObsBadgeItem(selectable = true)(
                                      obsResultToObsSummary(obs),
                                      idx
                                    )
                                  }
                                ),
                              provided.placeholder
                            )
                          )
                      }
                    }
                  )
                )
              ): VdomNode).some,
              (ReflexSplitter(propagate = true): VdomNode).some.filter(_ => asterisms.nonEmpty),
              (ReflexElement(minSize = 36, clazz = ExploreStyles.ObsTreeSection)(
                ReflexHandle()(
                  Header(block = true, clazz = ExploreStyles.ObsTreeHeader)("Asterisms")
                ),
                <.div(ExploreStyles.ObsTree)(
                  <.div(ExploreStyles.ObsScrollTree)(
                    asterisms.toTagMod { asterism =>
                      val asterismId    = asterism.id
                      val currIdx       = asterismIds.indexOf(asterismId)
                      val nextToSelect  = asterismIdxs.find(_._2 === currIdx + 1).map(_._1)
                      val prevToSelect  = asterismIdxs.find(_._2 === currIdx - 1).map(_._1)
                      val focusOnDelete = nextToSelect.orElse(prevToSelect)

                      val asterismTargets = asterism.targets.toList
                      val asterismObs     =
                        obsByPointing.get(PointingAsterismResult(asterismId).some).orEmpty

                      val expandedAsterismIds =
                        props.expandedIds.zoom(ExpandedIds.asterismIds)
                      val opIcon              =
                        (asterismObs.nonEmpty || asterismTargets.nonEmpty).fold(
                          Icon(
                            "chevron " + expandedAsterismIds.get
                              .exists(_ === asterismId)
                              .fold("down", "right")
                          )(^.cursor.pointer,
                            ^.onClick ==> { e: ReactEvent =>
                              e.stopPropagationCB >>
                                toggleExpanded(asterismId, expandedAsterismIds).runAsyncCB
                                  .asEventDefault(e)
                                  .void
                            }
                          ),
                          Icons.ChevronRight
                        )

                      val memberObsSelected = props.focused.get
                        .exists(f => asterismObs.map(obs => FocusedObs(obs.id)).exists(f === _))

                      Droppable(asterismId.toString) { case (provided, snapshot) =>
                        <.div(
                          provided.innerRef,
                          provided.droppableProps,
                          props.getListStyle(snapshot.isDraggingOver)
                        )(
                          Segment(
                            vertical = true,
                            clazz = ExploreStyles.ObsTreeGroup |+|
                              Option
                                .when(
                                  memberObsSelected || props.focused.get
                                    .exists(_ === FocusedAsterism(asterismId))
                                )(ExploreStyles.SelectedObsTreeGroup)
                                .orElse(
                                  Option.when(!state.get.dragging)(
                                    ExploreStyles.UnselectedObsTreeGroup
                                  )
                                )
                                .orEmpty
                          )(
                            ^.cursor.pointer,
                            ^.onClick --> props.focused
                              .set(FocusedAsterism(asterismId).some)
                              .runAsyncCB
                          )(
                            <.span(ExploreStyles.ObsTreeGroupHeader)(
                              <.span(
                                opIcon,
                                asterism.name.map(_.value),
                                ExploreStyles.ObsGroupTitle
                              ),
                              Button(
                                size = Small,
                                compact = true,
                                clazz = ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                                onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                                  e.stopPropagationCB >>
                                    deleteAsterism(asterismId, undoCtx, focusOnDelete).runAsyncCB
                              )(
                                Icons.Trash
                              ),
                              <.span(
                                ExploreStyles.ObsCount,
                                s"${asterismTargets.length} Tgts - ${asterismObs.length} Obs"
                              )
                            ),
                            TagMod.when(expandedAsterismIds.get.contains(asterismId))(
                              <.div(ExploreStyles.ObsTreeItem)(
                                SegmentGroup(
                                  asterismTargets
                                    .sortBy(_.name)
                                    .toTagMod(target =>
                                      Segment(basic = true,
                                              clazz = ExploreStyles.ObsTreeGroupHeader
                                      )(
                                        <.span(ExploreStyles.ObsGroupTitle)(target.name.value),
                                        Button(
                                          size = Small,
                                          compact = true,
                                          clazz =
                                            ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                                          onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                                            e.stopPropagationCB >>
                                              deleteTargetFromAsterism(target.id,
                                                                       asterismId,
                                                                       undoCtx
                                              ).runAsyncCB
                                        )(
                                          Icons.Trash
                                        )
                                      )
                                    )
                                ).when(asterismTargets.nonEmpty),
                                asterismObs.zipWithIndex.toTagMod { case (obs, idx) =>
                                  props.renderObsBadgeItem(selectable = true)(
                                    obsResultToObsSummary(obs),
                                    idx
                                  )
                                }
                              )
                            ),
                            provided.placeholder
                          )
                        )
                      }
                    }
                  )
                )
              ): VdomNode).some.filter(_ => asterisms.nonEmpty),
              (ReflexSplitter(propagate = true): VdomNode).some,
              (ReflexElement(size = 36,
                             minSize = 36,
                             clazz = ExploreStyles.ObsTreeSection,
                             withHandle = true
              )(
                ReflexWithHandle(reflexProvided =>
                  // End Asterism Tree - Start Unassigned Observations List
                  Droppable(UnassignedObsId) { case (provided, snapshot) =>
                    <.div(ExploreStyles.ObsUnassigned,
                          provided.innerRef,
                          provided.droppableProps,
                          props.getListStyle(snapshot.isDraggingOver)
                    )(
                      ReflexHandle(provided = reflexProvided)(
                        Header(
                          block = true,
                          clazz = ExploreStyles.ObsTreeHeader |+| ExploreStyles.ObsTreeGroupHeader
                        )(
                          <.span(ExploreStyles.ObsGroupTitle)("Unassigned Observations"),
                          <.span(ExploreStyles.ObsCount, s"${unassignedObs.length} Obs")
                        )
                      ),
                      <.div(ExploreStyles.ObsTree)(
                        <.div(ExploreStyles.ObsScrollTree) {

                          Segment(
                            vertical = true,
                            clazz = ExploreStyles.ObsTreeGroup
                          )(
                            unassignedObs.zipWithIndex.toTagMod { case (obs, idx) =>
                              props.renderObsBadgeItem(selectable = false)(
                                obsResultToObsSummary(obs),
                                idx
                              )
                            },
                            provided.placeholder
                          )
                        }
                      )
                    )
                  }
                )
              ): VdomNode).some
              // End Unassigned Observations List
            ).flatten: _*
          )
        )
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount { $ =>
        implicit val ctx = $.props.ctx

        val pointingsWithObs    = $.props.pointingsWithObs.get
        val expandedTargetIds   = $.props.expandedIds.zoom(ExpandedIds.targetIds)
        val expandedAsterismIds = $.props.expandedIds.zoom(ExpandedIds.asterismIds)

        // Unfocus if focused element is not in list.
        val unfocus =
          $.props.focused.get.map { focused =>
            $.props.focused
              .set(none)
              .whenA(focused match {
                case FocusedObs(oid)      => !pointingsWithObs.observations.contains(oid)
                case FocusedTarget(tid)   => !pointingsWithObs.targets.contains(tid)
                case FocusedAsterism(aid) => !pointingsWithObs.asterisms.contains(aid)
                case _                    => true // If focused on something else, unfocus too.
              })
          }.orEmpty

        // Expand target or asterism with focused observation
        val expandObservationObject =
          $.props.focused.get
            .collect { case FocusedObs(obsId) =>
              pointingsWithObs.observations
                .getElement(obsId)
                .flatMap(_.pointing.map(_ match {
                  case PointingTargetResult(targetId)     => expandedTargetIds.mod(_ + targetId)
                  case PointingAsterismResult(asterismId) => expandedAsterismIds.mod(_ + asterismId)
                }))
            }
            .flatten
            .orEmpty

        // Remove objects from expanded set which are no longer present.
        val removeTargets =
          (expandedTargetIds.get -- pointingsWithObs.targets.toList
            .map(_.id)).toNes
            .map(removedTargetIds => expandedTargetIds.mod(_ -- removedTargetIds.toSortedSet))
            .orEmpty

        val removeAsterisms =
          (expandedAsterismIds.get -- pointingsWithObs.asterisms.toList
            .map(_.id)).toNes
            .map(removedAsterismIds => expandedAsterismIds.mod(_ -- removedAsterismIds.toSortedSet))
            .orEmpty

        (unfocus >> expandObservationObject >> removeTargets >> removeAsterisms).runAsyncAndForgetCB
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
