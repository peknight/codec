package com.peknight.codec.sum

import cats.Foldable
import com.peknight.codec.Object

trait ObjectType[S]:
  type Obj
  def to(o: Obj): S
  def asObject(s: S): Option[Obj]
  def fromObject(o: Object[S]): Obj
  def toObject(o: Obj): Object[S]
  def isObject(s: S): Boolean = asObject(s).isDefined
  def singleton(key: String, value: S): Obj = fromObject(Object.singleton(key, value))
  def fromFoldable[F[_]](fields: F[(String, S)])(using Foldable[F]): Obj = fromObject(Object.fromFoldable(fields))
  def add(o: Obj, key: String, value: S): Obj = fromObject(toObject(o).add(key, value))
  def keys(o: Obj): Iterable[String] = toObject(o).keys
  def contains(o: Obj, key: String): Boolean = toObject(o).contains(key)
end ObjectType
object ObjectType:
  type Aux[S, O] = ObjectType[S] { type Obj = O }
  def apply[S](using objectType: ObjectType[S]): ObjectType[S] = objectType
  def objectType[S](f: Object[S] => S, g: S => Option[Object[S]]): ObjectType[S] =
    new ObjectType[S]:
      type Obj = Object[S]
      def to(o: Object[S]): S = f(o)
      def asObject(s: S): Option[Object[S]] = g(s)
      def fromObject(o: Object[S]): Object[S] = o
      def toObject(o: Object[S]): Object[S] = o
      override def singleton(key: String, value: S): Object[S] = Object.singleton(key, value)
      override def fromFoldable[F[_]](fields: F[(String, S)])(using Foldable[F]): Object[S] = Object.fromFoldable(fields)
      override def add(o: Object[S], key: String, value: S): Object[S] = o.add(key, value)
      override def keys(o: Object[S]): Iterable[String] = o.keys
      override def contains(o: Object[S], key: String): Boolean = o.contains(key)
  end objectType
end ObjectType
