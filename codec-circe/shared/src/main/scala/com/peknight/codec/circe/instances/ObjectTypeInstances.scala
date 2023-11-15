package com.peknight.codec.circe.instances

import cats.Foldable
import com.peknight.codec.ObjectType
import io.circe.{Json, JsonObject}

trait ObjectTypeInstances:
  given objectType: ObjectType[Json] with
    type Object = JsonObject
    def to(o: JsonObject): Json = Json.fromJsonObject(o)
    def asObject(s: Json): Option[JsonObject] = s.asObject
    def empty: JsonObject = JsonObject.empty
    def prepended(o: JsonObject, field: (String, Json)): JsonObject = field +: o
    def contains(o: JsonObject, key: String): Boolean = o.contains(key)
    override def singleton(key: String, value: Json): JsonObject = JsonObject.singleton(key, value)
    override def fromFoldable[F[_]: Foldable](f: F[(String, Json)]): JsonObject = JsonObject.fromFoldable(f)
  end objectType
end ObjectTypeInstances
object ObjectTypeInstances extends ObjectTypeInstances
