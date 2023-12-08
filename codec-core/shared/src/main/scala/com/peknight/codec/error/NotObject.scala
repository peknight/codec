package com.peknight.codec.error

import com.peknight.error.std.WrongType

case class NotObject[A](value: A) extends DecodingFailure[A] with WrongType:
  def map[B](f: A => B): DecodingFailure[B] = NotObject(f(value))
  def expectedType: String = "object"
end NotObject
