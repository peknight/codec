package com.peknight.codec.circe.derivation

import cats.Foldable
import com.peknight.codec.Object
import io.circe.{Json, JsonObject}

trait ObjectType extends com.peknight.codec.sum.ObjectType[Json]:
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
end ObjectType
object ObjectType extends ObjectType
