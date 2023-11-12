package com.peknight.codec.derivation

trait DecodeObject[T]:
  def isObject(t: T): Boolean
  def keys(t: T): Option[List[String]]
  def downField(t: T, field: String): T
  def decodeContains(t: T, key: String): Boolean
  def isNull(t: T): Boolean
end DecodeObject
