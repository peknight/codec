package com.peknight.codec.error

import com.peknight.error.Error.Label

case class NonEmptyObject[A](value: A, label: String) extends WrongType[A] with Label:
  def expectedType: String = "non-empty object"
end NonEmptyObject
