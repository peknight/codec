package com.peknight.codec.circe.instances

import cats.Foldable
import com.peknight.codec.derivation.CodecObject
import io.circe.{ACursor, Json, JsonObject}

trait CodecObjectInstances:
  given codecObject: CodecObject[Json, ACursor] with
    def empty: Json = Json.fromJsonObject(JsonObject.empty)
    def prepend(b: Json, field: (String, Json)): Json = b.asObject.fold(b)(j => Json.fromJsonObject(field +: j))
    def encodeContains(b: Json, key: String): Boolean = b.asObject.exists(_.contains(key))
    override def fromFoldable[F[_] : Foldable](f: F[(String, Json)]): Json =
      Json.fromJsonObject(JsonObject.fromFoldable(f))
    override def singleton(label: String, value: Json): Json = Json.fromJsonObject(JsonObject.singleton(label, value))
    def isObject(c: ACursor): Boolean = c.focus.exists(_.isObject)
    def keys(c: ACursor): Option[List[String]] = c.keys.map(_.toList)
    def downField(c: ACursor, field: String): ACursor = c.downField(field)
    def decodeContains(c: ACursor, key: String): Boolean = c.focus.exists(_.asObject.exists(_.contains(key)))
    def isNull(c: ACursor): Boolean = c.focus.exists(_.isNull)
  end codecObject

end CodecObjectInstances
object CodecObjectInstances extends CodecObjectInstances
