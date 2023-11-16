package com.peknight.codec.circe.derivation

import cats.Foldable
import io.circe.{Json, JsonObject}

trait ObjectType extends com.peknight.codec.derivation.ObjectType[Json]:
  type Object = JsonObject
  def to(o: JsonObject): Json = Json.fromJsonObject(o)
  def isNull(s: Json): Boolean = s.isNull
  override def isObject(s: Json): Boolean = s.isObject
  def asObject(s: Json): Option[JsonObject] = s.asObject
  def empty: JsonObject = JsonObject.empty
  def prepended(o: JsonObject, field: (String, Json)): JsonObject = field +: o
  def contains(o: JsonObject, key: String): Boolean = o.contains(key)
  def keys(o: JsonObject): Iterable[String] = o.keys
  override def singleton(key: String, value: Json): JsonObject = JsonObject.singleton(key, value)
  override def fromFoldable[F[_] : Foldable](f: F[(String, Json)]): JsonObject = JsonObject.fromFoldable(f)
end ObjectType
object ObjectType extends ObjectType
