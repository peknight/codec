package com.peknight.codec.cursor

import com.peknight.codec.sum.ArrayType

private[codec] trait ArrayCursor[S] extends SuccessCursor[S]:
  protected[this] def array: Vector[S]
  def indexValue: Int
  def parent: SuccessCursor[S]
  protected[this] def changed: Boolean
  protected[this] def arrayType: ArrayType[S]
  protected[this] def lastCursorValue: SuccessCursor[S]
  def lastCursor: Option[SuccessCursor[S]] = Some(lastCursorValue)
  def value: S = array(indexValue)
  def index: Option[Int] = Some(indexValue)
  def key: Option[String] = None
  private[this] def valuesExcept: Vector[S] = array.take(indexValue) ++ array.drop(indexValue + 1)

  def replace(newValue: S, cursor: SuccessCursor[S], op: Option[CursorOp]): SuccessCursor[S] =
    ArrayCursor(array.updated(indexValue, newValue), indexValue, parent, true, arrayType, cursor, op)

  def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S] =
    ArrayCursor(array, indexValue, parent, changed, arrayType, cursor, Some(op))

  def up: Cursor[S] = up(changed, parent, arrayType.to(array))

  def delete: Cursor[S] =
    parent.replace(arrayType.to(array), this, Some(CursorOp.DeleteGoParent))

  def left: Cursor[S] =
    if indexValue == 0 then fail(CursorOp.MoveLeft)
    else ArrayCursor(array, indexValue - 1, parent, changed, arrayType, this, Some(CursorOp.MoveLeft))

  def right: Cursor[S] =
    if indexValue == array.size - 1 then fail(CursorOp.MoveRight)
    else ArrayCursor(array, indexValue + 1, parent, changed, arrayType, this, Some(CursorOp.MoveRight))

  def field(k: String): Cursor[S] = fail(CursorOp.Field(k))
end ArrayCursor
private[codec] object ArrayCursor:
  def apply[S](array0: Vector[S], indexValue0: Int, parent0: SuccessCursor[S], changed0: Boolean,
               arrayType0: ArrayType[S], lastCursorValue0: SuccessCursor[S], lastOp0: Option[CursorOp])
  : ArrayCursor[S] =
    new ArrayCursor[S]:
      val array: Vector[S] = array0
      val indexValue: Int = indexValue0
      val parent: SuccessCursor[S] = parent0
      val changed: Boolean = changed0
      val arrayType: ArrayType[S] = arrayType0
      val lastCursorValue: SuccessCursor[S] = lastCursorValue0
      val lastOp: Option[CursorOp] = lastOp0
  end apply
end ArrayCursor