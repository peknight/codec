package com.peknight.codec.error

import com.peknight.error.Error.Label
import com.peknight.error.std.WrongType

case class NonEmptyObject[A](value: A, label: String) extends DecodingFailure[A] with WrongType with Label:
  def map[B](f: A => B): DecodingFailure[B] = NonEmptyObject(f(value), label)
  def expectedType: String = "non-empty object"
end NonEmptyObject
