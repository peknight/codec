package com.peknight.codec.obj

import cats.data.Kleisli
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import cats.syntax.show.*
import cats.{Applicative, Foldable, Monad, Semigroup, Show}

trait Object[S] extends Serializable:
  def applyUnsafe(key: String): S = apply(key).getOrElse(throw new NoSuchElementException(s"key not found: $key"))
  def apply(key: String): Option[S]
  def contains(key: String): Boolean
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def kleisli: Kleisli[Option, String, S] = Kleisli(apply)
  def keys: Iterable[String]
  def values: Iterable[S]
  def toMap: Map[String, S]
  def toIterable: Iterable[(String, S)]
  def toList: List[(String, S)] = toIterable.toList
  def toVector: Vector[(String, S)] = toIterable.toVector
  def add(key: String, value: S): Object[S]
  def prepended(field: (String, S)): Object[S]
  def +:(field: (String, S)): Object[S] = prepended(field)
  def remove(key: String): Object[S]
  def remove(keys: Seq[String]): Object[S]
  def traverse[F[_], T](f: S => F[T])(using Applicative[F]): F[Object[T]]
  def mapValues[T](f: S => T): Object[T]
  def filter(pred: ((String, S)) => Boolean): Object[S] = Object.fromIterable[S](toIterable.filter(pred))
  def filterKeys(pred: String => Boolean): Object[S] = filter(field => pred(field._1))
  def deepMerge(that: Object[S])(using Semigroup[S]): Object[S] =
    toIterable.foldLeft(that) {
      case (acc, (key, value)) =>
        that(key).fold(acc.add(key, value)) { r =>
          acc.add(key, value |+| r)
        }
    }
  def foldLeft[B](b: B)(f: (B, (String, S)) => B): B
  def foldLeftM[F[_]: Monad, B](b: B)(f: (B, (String, S)) => F[B]): F[B]
  override def toString: String = toIterable.map((key, value) => s"$key=$value").mkString("Object(", ",", ")")
end Object
object Object:
  def apply[S](fields: (String, S)*): Object[S] = fromIterable(fields)
  def fromFoldable[F[_], S](fields: F[(String, S)])(using Foldable[F]): Object[S] =
    fields.foldLeft(empty) {
      case (acc, (key, value)) => acc.add(key, value)
    }
  def fromIterable[S](fields: Iterable[(String, S)]): Object[S] =
    MapAndVectorObject[S](fields.toMap, fields.map(_._1).toVector)
  def fromMap[S](map: Map[String, S]): Object[S] = fromMapAndVector(map, map.keys.toVector)
  private def fromMapAndVector[S](map: Map[String, S], keys: Vector[String]): Object[S] =
    MapAndVectorObject[S](map, keys)
  def empty[S]: Object[S] = MapAndVectorObject[S](Map.empty, Vector.empty)
  def singleton[S](key: String, value: S): Object[S] = MapAndVectorObject[S](Map((key, value)), Vector(key))
  private class MapAndVectorObject[S](fields: Map[String, S], orderedKeys: Vector[String]) extends Object[S]:
    override def applyUnsafe(key: String): S = fields(key)
    def apply(key: String): Option[S] = fields.get(key)
    def contains(key: String): Boolean = fields.contains(key)
    def size: Int = fields.size
    def isEmpty: Boolean = fields.isEmpty
    def keys: Iterable[String] = orderedKeys
    def values: Iterable[S] = orderedKeys.map(key => fields(key))
    def toMap: Map[String, S] = fields
    def toIterable: Iterable[(String, S)] = orderedKeys.map(key => (key, fields(key)))
    def add(key: String, value: S): Object[S] =
      if fields.contains(key) then MapAndVectorObject[S](fields.updated(key, value), orderedKeys)
      else MapAndVectorObject[S](fields.updated(key, value), orderedKeys :+ key)
    def prepended(field: (String, S)): Object[S] =
      val (key, value) = field
      if fields.contains(key) then MapAndVectorObject[S](fields.updated(key, value), orderedKeys)
      else MapAndVectorObject[S](fields.updated(key, value), key +: orderedKeys)
    def remove(key: String): Object[S] = MapAndVectorObject[S](fields - key, orderedKeys.filterNot(_ == key))
    def remove(keys: Seq[String]): Object[S] = MapAndVectorObject[S](fields -- keys, orderedKeys.filterNot(keys.contains))
    def traverse[F[_], T](f: S => F[T])(using Applicative[F]): F[Object[T]] =
      orderedKeys.foldLeft(Map.empty[String, T].pure[F]) {
        case (acc, key) => (acc, f(fields(key))).mapN(_.updated(key, _))
      }.map(mappedFields => MapAndVectorObject[T](mappedFields, orderedKeys))
    def mapValues[T](f: S => T): Object[T] =
      MapAndVectorObject[T](
        fields.map {
          case (key, value) => (key, f(value))
        },
        orderedKeys
      )
    def foldLeft[B](b: B)(f: (B, (String, S)) => B): B =
      orderedKeys.foldLeft[B](b)((z, key) => f(z, (key, fields(key))))

    def foldLeftM[G[_]: Monad, B](b: B)(f: (B, (String, S)) => G[B]): G[B] =
      orderedKeys.foldLeftM[G, B](b)((z, key) => f(z, (key, fields(key))))
  end MapAndVectorObject
  given showObject[S](using Show[S]): Show[Object[S]] with
    def show(t: Object[S]): String =
      t.toIterable.map((key, value) => s"$key=${value.show}").mkString("Object(", ",", ")")
  end showObject
end Object
