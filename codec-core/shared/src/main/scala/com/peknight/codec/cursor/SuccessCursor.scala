package com.peknight.codec.cursor

import cats.Applicative
import cats.syntax.functor.*
import com.peknight.codec.sum.{ArrayType, ObjectType}

import scala.annotation.tailrec

trait SuccessCursor[S] extends Cursor[S]:
  def value: S
  def replace(newValue: S, cursor: Option[SuccessCursor[S]], op: Option[CursorOp]): SuccessCursor[S]
  def addOp(cursor: SuccessCursor[S], op: CursorOp): SuccessCursor[S]
  def withFocus(f: S => S): Cursor[S] = replace(f(value), Some(this), None)
  def withFocusM[F[_] : Applicative](f: S => F[S]): F[Cursor[S]] = f(value).map(replace(_, Some(this), None))
  def succeeded: Boolean = true
  def focus: Option[S] = Some(value)
  def values(using arrayType: ArrayType[S]): Option[Iterable[S]] = arrayType.asArray(value).map(arrayType.toArray)
  def keys(using objectType: ObjectType[S]): Option[Iterable[String]] = objectType.asObject(value).map(objectType.keys)
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
    
  
end SuccessCursor
