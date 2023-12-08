package com.peknight.codec.error

import com.peknight.error.std.WrongType

case class NotNull[A](value: A) extends DecodingFailure[A] with WrongType:
  def map[B](f: A => B): DecodingFailure[B] = NotNull(f(value))
  def expectedType: String = "'null' or '[]' or '{}'"
end NotNull
