package com.peknight.codec.reader

import cats.Show

case class Key(keys: Vector[String])
object Key:
  val empty: Key = Key(Vector.empty[String])
  def apply(key: String*): Key = Key(key.toVector)
  given showKey: Show[Key] with
    def show(t: Key): String = t.keys.mkString("Key(", ",", ")")
  end showKey
end Key
