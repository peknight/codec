package com.peknight.codec.derivation

trait DecodeObjectOps[T]:
  def isObject(t: T): Boolean
  def isNull(t: T): Boolean
  def contains(t: T, key: String): Boolean
  def keys(t: T): Option[List[String]]
  def downField(t: T, field: String): T
end DecodeObjectOps
