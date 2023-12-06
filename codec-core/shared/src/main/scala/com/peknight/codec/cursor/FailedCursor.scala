package com.peknight.codec.cursor

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.sum.{ArrayType, ObjectType}

class FailedCursor[S](val lastCursor: Option[SuccessCursor[S]], val lastOp: Option[CursorOp]) extends Cursor[S]:
  def incorrectFocus(using objectType: ObjectType[S], arrayType: ArrayType[S]): Boolean =
    (lastOp.exists(_.requiresObject) && !lastCursor.exists(c => objectType.isObject(c.value))) ||
      (lastOp.exists(_.requiresArray) && !lastCursor.exists(c => arrayType.isArray(c.value)))
  def missingField: Boolean =
    lastOp.exists {
      case _: CursorOp.Field | _: CursorOp.DownField => true
      case _ => false
    }
  def succeeded: Boolean = false
  def success: Option[SuccessCursor[S]] = None
  def focus: Option[S] = None
  def top: Option[S] = None
  def root: Option[SuccessCursor[S]] = lastCursor.flatMap(_.root)
  def withFocus(f: S => S): Cursor[S] = this
  def withFocusM[F[_] : Applicative](f: S => F[S]): F[Cursor[S]] = this.pure[F]

  def values: Option[Iterable[S]] = None

  def index: Option[Int] = None
  def keys: Option[Iterable[String]] = None
  def key: Option[String] = None
  def downArray: Cursor[S] = this
  def downField(k: String): Cursor[S] = this
  def downN(n: Int): Cursor[S] = this
  def up: Cursor[S] = this
  def left: Cursor[S] = this
  def right: Cursor[S] = this
  def last: Cursor[S] = this
  def delete: Cursor[S] = this
  def field(k: String): Cursor[S] = this
end FailedCursor
