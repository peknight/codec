package com.peknight.codec.circe.instances

import cats.Foldable
import com.peknight.codec.derivation.EncodeObjectOps
import io.circe.{Json, JsonObject}

trait EncodeObjectOpsInstances:
  given encodeObjectOps: EncodeObjectOps[Json] with
    def empty: Json = Json.fromJsonObject(JsonObject.empty)
    def prepended(b: Json, field: (String, Json)): Json = b.asObject.fold(b)(j => Json.fromJsonObject(field +: j))
    def contains(b: Json, key: String): Boolean = b.asObject.exists(_.contains(key))
    override def singleton(label: String, value: Json): Json = Json.fromJsonObject(JsonObject.singleton(label, value))
    override def fromFoldable[F[_] : Foldable](f: F[(String, Json)]): Json =
      Json.fromJsonObject(JsonObject.fromFoldable(f))
  end encodeObjectOps
end EncodeObjectOpsInstances
object EncodeObjectOpsInstances extends EncodeObjectOpsInstances
