// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Eq
import cats.Order._
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.kernel.Outcome
import cats.syntax.all._
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Constants
import explore.model.EmptySiderealTarget
import explore.model.TargetWithOptId
import explore.model.reusability._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.catalog.AngularSize
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.refined.*
import lucuma.ui.forms.FormInputEV
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.aladin._
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.modules.modal._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

final case class TargetSelectionPopup(
  programId:        Program.Id,
  trigger:          Button,
  onSelected:       TargetWithOptId => Callback
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetSelectionPopup](TargetSelectionPopup.component)

object TargetSelectionPopup {
  type Props = TargetSelectionPopup

  protected final case class Result(target: TargetSearchResult, priority: Int)
  protected object Result {
    implicit val eqResult: Eq[Result] = Eq.fromUniversalEquals
  }

  protected final case class SelectedTarget(
    target:      Target,
    source:      TargetSource[IO],
    resultIndex: Int,
    angularSize: Option[AngularSize]
  )

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // inputValue
    .useStateView("")
    // results
    .useState(
      SortedMap.empty[TargetSource[IO], NonEmptyList[Result]]
    )
    // searching
    .useState(false)
    // singleEffect
    .useSingleEffect
    // isOpen
    .useState(false)
    // selectedTarget
    .useState(none[SelectedTarget])
    // targetSources
    .useMemoBy((props, _, _, _, _, _, _) => props.ctx) { (props, _, _, _, _, _, _) => propsCtx =>
      implicit val ctx = propsCtx
      TargetSource.FromProgram[IO](props.programId) :: TargetSource.forAllCatalogs[IO]
    }
    // aladinRef
    .useMemo(())(_ => Ref.toScalaComponent(Aladin.component))
    .render {
      (
        props,
        inputValue,
        results,
        searching,
        singleEffect,
        isOpen,
        selectedTarget,
        targetSources,
        aladinRef
      ) =>
        implicit val ctx = props.ctx

        val cleanResults = selectedTarget.setState(none) >> results.setState(SortedMap.empty)

        val cleanState =
          inputValue.set("") >> searching.setState(false) >> cleanResults

        def addResults(source: TargetSource[IO], priority: Int)(
          targets:             List[TargetSearchResult]
        ): IO[Unit] =
          NonEmptyList
            .fromList(targets.map(t => Result(t, priority)))
            .map[IO[Unit]](nel =>
              results.modStateAsync(r =>
                r.get(source).fold(r + (source -> nel)) { case rs =>
                  r + (source ->
                    NonEmptyList
                      .fromListUnsafe {
                        // Remove duplicates, keeping the one with the highest priority (lowest value).
                        rs.filterNot(r =>
                          nel.exists(r0 => r.target === r0.target && r0.priority < r.priority)
                        ) ++
                          nel.filterNot(r0 =>
                            rs.exists(r => r.target === r0.target && r0.priority > r.priority)
                          )
                      }
                      .sortBy(r => (r.priority, r.target.target.name.value)))
                }
              )
            )
            .orEmpty

        def search(name: String): IO[Unit] =
          cleanResults.to[IO] >>
            NonEmptyString
              .from(name)
              .toOption
              .map(nonEmptyName =>
                searching.setStateAsync(true) >>
                  targetSources.value
                    .flatMap(source =>
                      source.searches(nonEmptyName).zipWithIndex.map { case (search, priority) =>
                        search >>= addResults(source, priority)
                      }
                    )
                    .parSequence_
                    .guaranteeCase {
                      // If it gets canceled, it's because another search has started
                      case Outcome.Canceled() => IO.unit
                      case _                  => searching.setStateAsync(false)
                    }
              )
              .orEmpty

        React.Fragment(
          props.trigger(^.onClick --> isOpen.setState(true)),
          Modal(
            as = <.form,      // This lets us sumbit on enter
            clazz = ExploreStyles.TargetSearchForm,
            actions = List(
              Button(size = Small, icon = true, negative = true)(
                Icons.Close,
                "Cancel"
              )(^.tpe := "button", ^.key := "input-cancel"),
              Button(
                size = Small,
                icon = true,
                positive = true,
                onClick = props.onSelected(TargetWithOptId(none, EmptySiderealTarget))
              )(
                Icons.New,
                "Create Empty Sidereal Target"
              )(^.tpe := "button", ^.key := "input-empty")
            ),
            centered = false, // Works better on iOS
            open = isOpen.value,
            closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
            dimmer = Dimmer.Blurring,
            size = ModalSize.Large,
            onOpen = cleanState,
            onClose = singleEffect.cancel.runAsync >> isOpen.setState(false) >> cleanState,
            header = ModalHeader(content = "Add Target"),
            content = ModalContent(
              <.span(ExploreStyles.TargetSearchTop)(
                <.span(ExploreStyles.TargetSearchInput)(
                  FormInputEV(
                    id = "name".refined,
                    value = inputValue,
                    // TODO Investigate if we can replicate SUI's "input with icon" styles (which use <i>) but using <svg>,
                    // so that they work with fontawesome.
                    // icon = Icons.Search,
                    // iconPosition = IconPosition.Left,
                    onTextChange = t =>
                      inputValue.set(t) >>
                        singleEffect.submit(IO.sleep(700.milliseconds) >> search(t)).runAsync,
                    loading = searching.value
                  )
                    .withMods(^.placeholder := "Name", ^.autoFocus := true)
                ),
                <.div(ExploreStyles.TargetSearchPreview)(
                  selectedTarget.value
                    .collect {
                      case SelectedTarget(Target.Sidereal(_, tracking, _, _), _, _, angSize) =>
                        (tracking.baseCoordinates, angSize)
                    }
                    .map { case (coordinates, angSize) =>
                      Aladin.component
                        .withRef(aladinRef)
                        .withKey(
                          selectedTarget.value.foldMap(t => s"${t.source}-${t.resultIndex}")
                        )(
                          Aladin(
                            ExploreStyles.TargetSearchAladin,
                            showReticle = false,
                            showLayersControl = false,
                            target = Coordinates.fromHmsDms.reverseGet(coordinates),
                            fov = angSize
                              .map(m =>
                                Angle.microarcseconds
                                  .modify(Constants.AngleSizeFovFactor)(m.majorAxis)
                              )
                              .getOrElse(Constants.InitialFov): Angle,
                            showGotoControl = false
                          )
                        )
                    }
                    .whenDefined
                )
              ),
              SegmentGroup(raised = true, clazz = ExploreStyles.TargetSearchResults)(
                results.value.map { case (source, sourceResults) =>
                  Segment(
                    <.div(
                      Header(size = Small)(
                        s"${source.name} (${showCount(sourceResults.length, "result")})"
                      ),
                      <.div(ExploreStyles.TargetSearchResultsSource)(
                        TargetSelectionTable(
                          sourceResults.toList.map(_.target),
                          onSelected = t =>
                            props.onSelected(t.targetWithOptId) >>
                              isOpen.setState(false) >>
                              cleanState,
                          selectedIndex = selectedTarget.value
                            .filter(_.source === source)
                            .map(_.resultIndex),
                          onClick = (result, index) =>
                            selectedTarget.setState(
                              if (
                                selectedTarget.value
                                  .exists(st => st.source === source && st.resultIndex === index)
                              )
                                none
                              else
                                SelectedTarget(
                                  result.target,
                                  source,
                                  index,
                                  result.angularSize
                                ).some
                            )
                        )
                      )
                    )
                  )
                }.toTagMod
              ).when(results.value.nonEmpty)
            )
          )(
            ^.autoComplete.off,
            ^.onSubmit ==> (e =>
              e.preventDefaultCB >>
                singleEffect
                  .submit(search(inputValue.get))
                  .runAsync
                  .whenA(!searching.value)
            )
          )
        )
    }
}
