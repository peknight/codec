package com.peknight.codec.cursor

import cats.Applicative
import cats.syntax.functor.*
import com.peknight.codec.sum.{ArrayType, ObjectType}

import scala.annotation.tailrec

trait SuccessCursor[S] extends Cursor[S]:
  def value: S
  def replace(newValue: S, cursor: SuccessCursor[S], op: Option[CursorOp]): SuccessCursor[S]
  def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S]
  def withFocus(f: S => S): Cursor[S] = replace(f(value), this, None)
  def withFocusM[F[_] : Applicative](f: S => F[S]): F[Cursor[S]] = f(value).map(replace(_, this, None))
  def succeeded: Boolean = true
  def success: Option[SuccessCursor[S]] = Some(this)
  def focus: Option[S] = Some(value)
  def values(using ArrayType[S]): Option[Iterable[S]] = ArrayType[S].asArray(value)
  def keys(using objectType: ObjectType[S]): Option[Iterable[String]] = objectType.asObject(value).map(objectType.keys)
  protected[this] def up[A](changed: Boolean, parent: SuccessCursor[S], s: S): Cursor[S] =
    if !changed then parent.addOp(this, CursorOp.MoveUp)
    else parent.replace(s, this, Some(CursorOp.MoveUp))

  def top: Option[S] =
    @tailrec def go(cursor: SuccessCursor[S]): Option[S] =
      cursor match
        case topCursor: TopCursor[S] => Some(topCursor.value)
        case _ => go(cursor.up.asInstanceOf[SuccessCursor[S]])
    go(this)

  def root: Option[SuccessCursor[S]] =
    @tailrec def go(cursor: SuccessCursor[S]): Option[SuccessCursor[S]] =
      cursor match
        case topCursor: TopCursor[S] => Some(topCursor)
        case _ => go(cursor.up.asInstanceOf[SuccessCursor[S]])
    go(this)

  def downArray(using ArrayType[S]): Cursor[S] =
    ArrayType[S].asArray(value).filter(_.nonEmpty) match
      case Some(values) =>
        ArrayCursor(values, 0, this, false, ArrayType[S], this,
          Some(CursorOp.DownArray))
      case _ => fail(CursorOp.DownArray)

  def find(p: S => Boolean): Cursor[S] =
    @tailrec def go(cursor: Cursor[S]): Cursor[S] =
      cursor match
        case success: SuccessCursor[S] => if p(success.value) then success else go(success.right)
        case other => other
    go(this)

  def downField(k: String)(using objectType: ObjectType[S]): Cursor[S] =
    objectType.asObject(value) match
      case Some(o) if objectType.contains(o, k) =>
        ObjectCursor[S, objectType.Obj](o, k, this, false, objectType, this,
          Some(CursorOp.DownField(k)))
      case _ => fail(CursorOp.DownField(k))

  def downN(n: Int)(using ArrayType[S]): Cursor[S] =
    ArrayType[S].asArray(value) match
      case Some(values) if n >= 0 && values.lengthCompare(n) > 0 =>
        ArrayCursor(values, n, this, false, ArrayType[S], this, Some(CursorOp.DownN(n)))
      case _ => fail(CursorOp.DownN(n))

  protected[this] def fail(op: CursorOp): Cursor[S] = FailedCursor(this, op)
end SuccessCursor
