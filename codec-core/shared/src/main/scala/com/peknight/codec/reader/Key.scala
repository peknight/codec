package com.peknight.codec.reader

case class Key(keys: Vector[String])
object Key:
  val empty: Key = Key(Vector.empty[String])
end Key
