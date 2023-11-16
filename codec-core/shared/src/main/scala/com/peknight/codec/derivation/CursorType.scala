package com.peknight.codec.derivation

trait CursorType[T]:
  type Sum
  def focus(t: T): Option[Sum]
  def downField(t: T, k: String): T
end CursorType
object CursorType:
  type Aux[T, S] = CursorType[T] { type Sum = S }
end CursorType