package com.peknight.codec

import cats.data.Kleisli
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import cats.{Applicative, Foldable, Semigroup}

trait Object[S]:
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
  def traverse[F[_]](f: S => F[S])(using Applicative[F]): F[Object[S]]
  def mapValues(f: S => S): Object[S]
  def filter(pred: ((String, S)) => Boolean): Object[S] = Object.fromIterable[S](toIterable.filter(pred))
  def filterKeys(pred: String => Boolean): Object[S] = filter(field => pred(field._1))
  def deepMerge(that: Object[S])(using Semigroup[S]): Object[S] =
    toIterable.foldLeft(that) {
      case (acc, (key, value)) =>
        that(key).fold(acc.add(key, value)) { r =>
          acc.add(key, value |+| r)
        }
    }
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
  private[this] def fromMapAndVector[S](map: Map[String, S], keys: Vector[String]): Object[S] =
    MapAndVectorObject[S](map, keys)
  def empty[S]: Object[S] = MapAndVectorObject[S](Map.empty, Vector.empty)
  def singleton[S](key: String, value: S): Object[S] = MapAndVectorObject[S](Map((key, value)), Vector(key))
  private[this] class MapAndVectorObject[S](fields: Map[String, S], orderedKeys: Vector[String]) extends Object[S]:
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
    def traverse[F[_]](f: S => F[S])(using Applicative[F]): F[Object[S]] =
      orderedKeys.foldLeft(Map.empty[String, S].pure[F]) {
        case (acc, key) => (acc, f(fields(key))).mapN(_.updated(key, _))
      }.map(mappedFields => MapAndVectorObject[S](mappedFields, orderedKeys))
    def mapValues(f: S => S): Object[S] =
      MapAndVectorObject[S](
        fields.map {
          case (key, value) => (key, f(value))
        },
        orderedKeys
      )
  end MapAndVectorObject
end Object
