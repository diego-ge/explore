// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.sequence._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment

final case class ManualSequenceTables(config: ManualConfig)
    extends ReactFnProps[ManualSequenceTables](ManualSequenceTables.component)

object ManualSequenceTables {
  type Props = ManualSequenceTables

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .render { props =>
        <.div(^.height := "100%", ^.overflow.auto)(
          Segment(
            SequenceTable.bracketDef,
            <.div(
              Header("Acquisition"),
              SequenceTable(props.config.acquisition),
              Header("Science"),
              SequenceTable(props.config.science)
            )
          )
        )
      }
}
