package com.peknight.codec.error

case class NotObject[A](value: A) extends WrongType[A]:
  def expectedType: String = "object"
end NotObject
