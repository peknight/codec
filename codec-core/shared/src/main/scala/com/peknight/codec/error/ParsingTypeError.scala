package com.peknight.codec.error

import com.peknight.error.std.JavaThrowable

import scala.reflect.ClassTag

trait ParsingTypeError[A, C] extends WrongClassTag[A, C] with JavaThrowable[Throwable]
object ParsingTypeError:
  private[this] case class ParsingTypeError[A, C](value: A, throwable: Throwable, expectedClassTag: ClassTag[C])
    extends com.peknight.codec.error.ParsingTypeError[A, C]:
    def map[B](f: A => B): DecodingFailure[B] = ParsingTypeError(f(value), throwable, expectedClassTag)
  end ParsingTypeError
  def apply[A, C](value: A, throwable: Throwable)(using classTag: ClassTag[C])
  : com.peknight.codec.error.ParsingTypeError[A, C] =
    ParsingTypeError(value, throwable, classTag)
end ParsingTypeError
