package com.peknight.codec.cursor


import com.peknight.codec.sum.ObjectType

class ObjectCursor[S, O](obj: O, keyValue: String, parent: SuccessCursor[S], changed: Boolean,
                         objectType: ObjectType.Aux[S, O], lastCursorValue: SuccessCursor[S],
                         val lastOp: Option[CursorOp]) extends SuccessCursor[S]:
  def lastCursor: Option[SuccessCursor[S]] = Some(lastCursorValue)
  def value: S = objectType.applyUnsafe(obj, keyValue)
  def index: Option[Int] = None
  def key: Option[String] = Some(keyValue)

  def replace(newValue: S, cursor: SuccessCursor[S], op: Option[CursorOp]): SuccessCursor[S] =
    ObjectCursor(objectType.add(obj, keyValue, newValue), keyValue, parent, true, objectType, cursor, op)

  def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S] =
    ObjectCursor(obj, keyValue, parent, changed, objectType, cursor, Some(op))

  def up: Cursor[S] = up(changed, parent, objectType.to(obj))

  def delete: Cursor[S] =
    parent.replace(objectType.to(objectType.remove(obj, keyValue)), this, Some(CursorOp.DeleteGoParent))

  def field(k: String): Cursor[S] =
    if !objectType.contains(obj, k) then fail(CursorOp.Field(k))
    else ObjectCursor(obj, k, parent, changed, objectType, this, Some(CursorOp.Field(k)))

  def left: Cursor[S] = fail(CursorOp.MoveLeft)
  def right: Cursor[S] = fail(CursorOp.MoveRight)
end ObjectCursor

