package explore

package object undo {
  type Mod[F[_], M] = (M => M) => F[Unit]

  type UndoStack[F[_], M] = List[Restorer[F, M]]
}
