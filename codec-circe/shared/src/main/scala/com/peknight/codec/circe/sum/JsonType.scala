package com.peknight.codec.circe.sum

import cats.{Foldable, Id}
import com.peknight.codec.circe.iso.numberIsomorphism
import com.peknight.codec.number.Number
import com.peknight.codec.obj.Object
import com.peknight.codec.sum.*
import io.circe.{Json, JsonObject}

trait JsonType extends StringType[Json] with ArrayType[Json] with ObjectType[Json] with NullType[Json]
  with NumberType[Json] with BooleanType[Json]:
  def to(s: String): Json = Json.fromString(s)
  def asString(s: Json): Option[String] = s.asString
  override def isString(s: Json): Boolean = s.isString

  def to(a: Vector[Json]): Json = Json.fromValues(a)
  def asArray(s: Json): Option[Vector[Json]] = s.asArray
  override def isArray(s: Json): Boolean = s.isArray

  type Obj = JsonObject
  def to(o: JsonObject): Json = Json.fromJsonObject(o)
  def asObject(s: Json): Option[JsonObject] = s.asObject
  def fromObject(o: Object[Json]): JsonObject = JsonObject.fromIterable(o.toIterable)
  def toObject(o: JsonObject): Object[Json] = Object.fromIterable(o.toIterable)
  override def isObject(s: Json): Boolean = s.isObject
  override def singleton(key: String, value: Json): JsonObject = JsonObject.singleton(key, value)
  override def fromFoldable[F[_]](fields: F[(String, Json)])(using Foldable[F]): JsonObject =
    JsonObject.fromFoldable(fields)
  override def add(o: JsonObject, key: String, value: Json): JsonObject = o.add(key, value)
  override def keys(o: JsonObject): Iterable[String] = o.keys
  override def contains(o: JsonObject, key: String): Boolean = o.contains(key)
  override def remove(o: JsonObject, key: String): JsonObject = o.remove(key)
  override def remove(o: JsonObject, keys: Seq[String]): JsonObject = keys.foldLeft(o)(_.remove(_))
  override def isEmpty(o: JsonObject): Boolean = o.isEmpty
  override def applyUnsafe(o: JsonObject, key: String): Json =
    o(key).getOrElse(throw new NoSuchElementException(s"key not found: $key"))
  override def toList(o: JsonObject): List[(String, Json)] = o.toList

  def unit: Json = Json.Null
  def asNull(s: Json): Option[Unit] = s.asNull
  override def isNull(s: Json): Boolean = s.isNull

  def to(n: Number): Json = Json.fromJsonNumber(numberIsomorphism[Id].to(n))
  def asNumber(s: Json): Option[Number] = s.asNumber.map(numberIsomorphism[Id].from)
  override def isNumber(s: Json): Boolean = s.isNumber

  def to(b: Boolean): Json = Json.fromBoolean(b)

  def asBoolean(s: Json): Option[Boolean] = s.asBoolean
  override def isBoolean(s: Json): Boolean = s.isBoolean
end JsonType
object JsonType extends JsonType