// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import cats.effect.IO
import crystal.ViewF
import crystal.ViewOptF
import explore.model.AppContext
import explore.undo.UndoContext
import japgolly.scalajs.react.callback.CallbackTo

package explore {

  trait ShorthandTypes {
    type AppContextIO = AppContext[IO]
    type View[A]      = ViewF[CallbackTo, A]
    type ViewOpt[A]   = ViewOptF[CallbackTo, A]
    type UndoCtx[A]   = UndoContext[CallbackTo, IO, A]
  }

}

package object explore extends explore.ShorthandTypes
