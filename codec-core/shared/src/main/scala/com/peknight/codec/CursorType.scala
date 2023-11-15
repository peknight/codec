package com.peknight.codec

trait CursorType[T]:
  type Sum
  def focus(t: T): Option[Sum]
end CursorType
object CursorType:
  type Aux[T, S] = CursorType[T] { type Sum = S }
end CursorType