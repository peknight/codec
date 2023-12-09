package com.peknight.codec.cursor

import cats.Applicative
import cats.syntax.applicative.*
import com.peknight.codec.sum.{ArrayType, ObjectType}

trait FailedCursor[S] extends Cursor[S]:
  protected[this] def lastCursorValue: SuccessCursor[S]
  protected[this] def lastOpValue: CursorOp
  protected[codec] def lastCursor: Option[SuccessCursor[S]] = Some(lastCursorValue)
  protected[codec] def lastOp: Option[CursorOp] = Some(lastOpValue)
  def incorrectFocusO(using ObjectType[S]): Boolean =
    lastOpValue.requiresObject && !ObjectType[S].isObject(lastCursorValue.value)
  def incorrectFocusA(using ArrayType[S]): Boolean =
    lastOpValue.requiresArray && !ArrayType[S].isArray(lastCursorValue.value)
  def incorrectFocus(using ObjectType[S], ArrayType[S]): Boolean = incorrectFocusO || incorrectFocusA
  def missingField: Boolean =
    lastOpValue match
      case _: CursorOp.Field | _: CursorOp.DownField => true
      case _ => false
  def succeeded: Boolean = false
  def success: Option[SuccessCursor[S]] = None
  def focus: Option[S] = None
  def top: Option[S] = None
  def root: Option[SuccessCursor[S]] = lastCursorValue.root
  def withFocus(f: S => S): Cursor[S] = this
  def withFocusM[F[_] : Applicative](f: S => F[S]): F[Cursor[S]] = this.pure[F]

  def values(using ArrayType[S]): Option[Iterable[S]] = None

  def index: Option[Int] = None
  def keys(using ObjectType[S]): Option[Iterable[String]] = None
  def key: Option[String] = None
  def downArray(using ArrayType[S]): Cursor[S] = this
  def downField(k: String)(using ObjectType[S]): Cursor[S] = this
  def downN(n: Int)(using ArrayType[S]): Cursor[S] = this
  def up: Cursor[S] = this
  def left: Cursor[S] = this
  def right: Cursor[S] = this
  def last: Cursor[S] = this
  def delete: Cursor[S] = this
  def field(k: String): Cursor[S] = this
end FailedCursor
object FailedCursor:
  def apply[S](lastCursorValue0: SuccessCursor[S], lastOpValue0: CursorOp): FailedCursor[S] =
    new FailedCursor[S]:
      val lastCursorValue: SuccessCursor[S] = lastCursorValue0
      val lastOpValue: CursorOp = lastOpValue0
  end apply
end FailedCursor