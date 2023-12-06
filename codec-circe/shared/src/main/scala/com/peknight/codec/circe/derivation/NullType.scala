package com.peknight.codec.circe.derivation

import io.circe.Json

trait NullType extends com.peknight.codec.sum.NullType[Json]:
  def unit: Json = Json.Null
  def asNull(s: Json): Option[Unit] = s.asNull
  override def isNull(s: Json): Boolean = s.isNull
end NullType
object NullType extends NullType
