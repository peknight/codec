package com.peknight.codec.cursor

import scala.annotation.tailrec

trait Cursor[S]:
  def focus: Option[S]
  def lastCursor: Option[SuccessCursor[S]]
  def lastOp: Option[CursorOp]
  final def history: List[CursorOp] =
    @tailrec def go(cursor: Cursor[S], acc: List[CursorOp]): List[CursorOp] =
      (cursor.lastCursor, cursor.lastOp) match
        case (Some(c), Some(op)) => go(c, op :: acc)
        case (Some(c), None) => go(c, acc)
        case (None, Some(op)) => op :: acc
        case _ => acc
    go(this, List.empty[CursorOp]).reverse

  def succeeded: Boolean
  final def failed: Boolean = !succeeded

end Cursor

