package explore.undo

package object v2 {
  type Mod[F[_], M] = (M => M) => F[Unit]

  type UndoStack[F[_], M] = List[Restorer2[F, M]]
}
