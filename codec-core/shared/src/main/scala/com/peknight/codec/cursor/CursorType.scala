package com.peknight.codec.cursor

trait CursorType[T]:
  type Sum
  def focus(t: T): Option[Sum]
  def downField(t: T, k: String): T
  def pathString(t: T): String
  def history(t: T): List[CursorOp]
end CursorType
object CursorType:
  type Aux[T, S] = CursorType[T] { type Sum = S }
end CursorType