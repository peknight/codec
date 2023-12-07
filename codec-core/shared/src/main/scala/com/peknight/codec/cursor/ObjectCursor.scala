package com.peknight.codec.cursor


import com.peknight.codec.sum.ObjectType

private[codec] trait ObjectCursor[S, O] extends SuccessCursor[S]:
  protected[this] def obj: O
  def keyValue: String
  def parent: SuccessCursor[S]
  protected[this] def changed: Boolean
  protected[this] def objectType: ObjectType.Aux[S, O]
  protected[this] def lastCursorValue: SuccessCursor[S]
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
private[codec] object ObjectCursor:
  def apply[S, O](obj0: O, keyValue0: String, parent0: SuccessCursor[S], changed0: Boolean,
                  objectType0: ObjectType.Aux[S, O], lastCursorValue0: SuccessCursor[S], lastOp0: Option[CursorOp])
  : ObjectCursor[S, O] =
    new ObjectCursor[S, O]:
      val obj: O = obj0
      val keyValue: String = keyValue0
      val parent: SuccessCursor[S] = parent0
      val changed: Boolean = changed0
      val objectType: ObjectType.Aux[S, O] = objectType0
      val lastCursorValue: SuccessCursor[S] = lastCursorValue0
      val lastOp: Option[CursorOp] = lastOp0
  end apply
end ObjectCursor

