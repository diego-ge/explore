// // Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// // For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.components.undo

// import cats.effect.Async
// import cats.effect.IO
// import crystal.react.implicits._
// import crystal.react.reuse._
// import explore.undo._
// import japgolly.scalajs.react._
// import japgolly.scalajs.react.vdom.html_<^._
// import react.common.ReactProps
// import crystal.ViewF
// import explore.implicits._
// import explore.model.reusability._

// final case class UndoRegion2[M](
//   stacksView: View[UndoStacks[IO, M]],
//   renderer:   Undoer.Context[IO, M] ==> VdomNode
// ) extends ReactProps(UndoRegion2.component)
//     with UndoRegion2.Props[IO, M]

// object UndoRegion2 {
//   protected trait Props[F[_], M] {
//     val stacksView: ViewF[IO, UndoStacks[F, M]]
//     val renderer: Undoer.Context[F, M] ==> VdomNode
//   }

//   implicit protected def propsReuse[F[_], M]: Reusability[Props[F, M]] =
//     Reusability.by(p => (p.stacksView, p.renderer))

//   protected case class Backend[F[_], M](
//     undoer: Undoer.Context[F, M],
//     $      : BackendScope[Props[F, M], Unit]
//   ) {
//     def render(props: Props[F, M]): VdomNode =
//       // println(s"UNDO STACK: [${state.undoStack}]")
//       // println(s"REDO STACK: [${state.redoStack}]")
//       props.renderer(undoer.context(props.stacksView.get))
//   }

//   protected def componentBuilder[M] =
//     ScalaComponent
//       .builder[Props[IO, M]]
//       // .backend($ => Backend($))
//       .renderBackend[Backend[IO, M]]
//       .configure(Reusability.shouldComponentUpdate)
//       .build

//   val component = componentBuilder[Any]
// }
