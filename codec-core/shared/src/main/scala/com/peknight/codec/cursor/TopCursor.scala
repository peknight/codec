package com.peknight.codec.cursor

class TopCursor[S](val value: S, val lastCursor: Option[SuccessCursor[S]], val lastOp: Option[CursorOp])
  extends SuccessCursor[S]:
  def index: Option[Int] = None
  def key: Option[String] = None

  def replace(newValue: S, cursor: SuccessCursor[S], op: Option[CursorOp]): SuccessCursor[S] =
    TopCursor(newValue, Some(cursor), op)

  def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S] = TopCursor(value, Some(cursor), Some(op))

  def up: Cursor[S] = fail(CursorOp.MoveUp)
  def delete: Cursor[S] = fail(CursorOp.DeleteGoParent)
  def left: Cursor[S] = fail(CursorOp.MoveLeft)
  def right: Cursor[S] = fail(CursorOp.MoveRight)
  def field(k: String): Cursor[S] = fail(CursorOp.Field(k))
end TopCursor
