package com.peknight.codec.circe.instances

import cats.Foldable
import com.peknight.codec.derivation.CodecObjectOps
import io.circe.{ACursor, Json, JsonObject}

trait CodecObjectOpsInstances:
  given codecObjectOps: CodecObjectOps[Json, ACursor] with
    def encodeContains(b: Json, key: String): Boolean = b.asObject.exists(_.contains(key))
    def empty: Json = Json.fromJsonObject(JsonObject.empty)
    override def singleton(label: String, value: Json): Json = Json.fromJsonObject(JsonObject.singleton(label, value))
    override def fromFoldable[F[_] : Foldable](f: F[(String, Json)]): Json =
      Json.fromJsonObject(JsonObject.fromFoldable(f))
    def prepend(b: Json, field: (String, Json)): Json = b.asObject.fold(b)(j => Json.fromJsonObject(field +: j))
    def isObject(c: ACursor): Boolean = c.focus.exists(_.isObject)
    def isNull(c: ACursor): Boolean = c.focus.exists(_.isNull)
    def decodeContains(c: ACursor, key: String): Boolean = c.focus.exists(_.asObject.exists(_.contains(key)))
    def keys(c: ACursor): Option[List[String]] = c.keys.map(_.toList)
    def downField(c: ACursor, field: String): ACursor = c.downField(field)
  end codecObjectOps

end CodecObjectOpsInstances
object CodecObjectOpsInstances extends CodecObjectOpsInstances
