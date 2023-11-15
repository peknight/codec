package com.peknight.codec

import cats.{Eval, Foldable}
import cats.syntax.foldable.*

trait ObjectType[S]:
  type Object
  def to(o: Object): S
  def asObject(s: S): Option[Object]
  def empty: Object
  def prepended(o: Object, field: (String, S)): Object
  def contains(o: Object, key: String): Boolean
  def singleton(key: String, value: S): Object = prepended(empty, (key, value))
  def fromFoldable[F[_]: Foldable](f: F[(String, S)]): Object =
    f.foldRight(Eval.later(empty))((field, eval) => eval.map(o => prepended(o, field))).value
end ObjectType
object ObjectType:
  type Aux[S, O] = ObjectType[S] { type Object = O }
  def apply[S](using objectType: ObjectType[S]): ObjectType[S] = objectType
end ObjectType
