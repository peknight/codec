package com.peknight.codec.sum

import cats.Foldable
import com.peknight.codec.obj.Object

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
  def prepended(o: Obj, key: String, value: S): Obj = fromObject(toObject(o).prepended(key, value))
  def keys(o: Obj): Iterable[String] = toObject(o).keys
  def contains(o: Obj, key: String): Boolean = toObject(o).contains(key)
  def remove(o: Obj, key: String): Obj = fromObject(toObject(o).remove(key))
  def remove(o: Obj, keys: Seq[String]): Obj = fromObject(toObject(o).remove(keys))
  def isEmpty(o: Obj): Boolean = toObject(o).isEmpty
  def applyUnsafe(o: Obj, key: String): S = toObject(o).applyUnsafe(key)
  def toList(o: Obj): List[(String, S)] = toObject(o).toList
end ObjectType
object ObjectType:
  type Aux[S, O] = ObjectType[S] { type Obj = O }
  def apply[S](using objectType: ObjectType[S]): ObjectType[S] = objectType
  def apply[S](f: Object[S] => S, g: S => Option[Object[S]]): ObjectType[S] =
    new ObjectType[S]:
      type Obj = Object[S]
      def to(o: Object[S]): S = f(o)
      def asObject(s: S): Option[Object[S]] = g(s)
      def fromObject(o: Object[S]): Object[S] = o
      def toObject(o: Object[S]): Object[S] = o
      override def singleton(key: String, value: S): Object[S] = Object.singleton(key, value)
      override def fromFoldable[F[_]](fields: F[(String, S)])(using Foldable[F]): Object[S] = Object.fromFoldable(fields)
      override def add(o: Object[S], key: String, value: S): Object[S] = o.add(key, value)
      override def prepended(o: Object[S], key: String, value: S): Object[S] = o.prepended(key, value)
      override def keys(o: Object[S]): Iterable[String] = o.keys
      override def contains(o: Object[S], key: String): Boolean = o.contains(key)
      override def remove(o: Object[S], key: String): Object[S] = o.remove(key)
      override def remove(o: Object[S], keys: Seq[String]): Object[S] = o.remove(keys)
      override def isEmpty(o: Object[S]): Boolean = o.isEmpty
      override def applyUnsafe(o: Object[S], key: String): S = o.applyUnsafe(key)
      override def toList(o: Object[S]): List[(String, S)] = o.toList
  end apply
end ObjectType
