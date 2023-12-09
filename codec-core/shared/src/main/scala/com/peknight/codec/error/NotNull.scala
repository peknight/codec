package com.peknight.codec.error

import com.peknight.error.std.WrongType

case class NotNull[A](value: A) extends DecodingFailure[A] with WrongType:
  def map[B](f: A => B): DecodingFailure[B] = NotUnit(f(value))
  def expectedType: String = "null"
end NotNull
