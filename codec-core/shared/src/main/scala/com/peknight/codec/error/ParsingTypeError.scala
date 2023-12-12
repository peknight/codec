package com.peknight.codec.error

import com.peknight.error.std.JavaThrowable

import scala.reflect.ClassTag

case class ParsingTypeError[A](throwable: Throwable)(using classTag: ClassTag[A])
  extends WrongClassTag[A] with JavaThrowable[Throwable]:
  override val expectedClassTag: ClassTag[A] = classTag
end ParsingTypeError
