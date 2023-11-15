package com.peknight.codec.circe.instances

import com.peknight.codec.derivation.DecodeObjectOps
import io.circe.ACursor

trait DecodeObjectOpsInstances:
  given decodeObjectOps: DecodeObjectOps[ACursor] with
    def isObject(c: ACursor): Boolean = c.focus.exists(_.isObject)
    def isNull(c: ACursor): Boolean = c.focus.exists(_.isNull)
    def contains(c: ACursor, key: String): Boolean = c.focus.exists(_.asObject.exists(_.contains(key)))
    def keys(c: ACursor): Option[List[String]] = c.keys.map(_.toList)
    def downField(c: ACursor, field: String): ACursor = c.downField(field)
  end decodeObjectOps
end DecodeObjectOpsInstances
object DecodeObjectOpsInstances extends DecodeObjectOpsInstances
